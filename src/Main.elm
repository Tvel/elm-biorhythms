module Main exposing (..)

import Biorhythm exposing (Biorhythm)
import Biorhythm.Chart exposing (BiorhythmData)
import Biorhythm.PeroidCycle exposing (PeriodCycle)
import Browser
import DateCalc exposing (BirthDate)
import DateTime exposing (DateTime)
import Html exposing (Html, button, div, h2, h6, input, li, option, p, select, span, text, ul)
import Html.Attributes exposing (checked, disabled, placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Encode as E
import Maybe
import Month
import People exposing (Person)
import Result
import Storage
import Task
import Time exposing (Month(..), Zone, ZoneName(..))


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


type alias Form =
    { day : String, month : String, year : String, name : String }


type alias Model =
    { value : Int
    , form : Form
    , zone : Time.Zone
    , time : DateTime
    , periodCycle : PeriodCycle
    , people : List Person
    }


defaultModel : Model
defaultModel =
    { value = 0
    , form = Form "11" "Jul" "1990" "Tosil"
    , zone = Time.utc
    , time = DateTime.fromPosix (Time.millisToPosix 0)
    , periodCycle = Biorhythm.PeroidCycle.accurateCycle
    , people = [ { day = 11, month = Jul, year = 1990, name = "Tosil" } ]
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case Storage.decode flags of
        Ok saveModel ->
            { defaultModel | periodCycle = Biorhythm.PeroidCycle.fromString saveModel.periodCycle
            , people = saveModel.people
            }

        Err _ ->
            defaultModel
    , Task.perform AdjustTimeZone Time.here
    )


modelToSaveModel : Model -> Storage.SaveModel
modelToSaveModel model =
    Storage.SaveModel (Biorhythm.PeroidCycle.toString model.periodCycle) model.people

setTimeToNow: Cmd Msg
setTimeToNow =
    Task.perform SetTimeNow Time.now


type Msg
    = AdjustTimeZone Time.Zone
    | SetTimeNow Time.Posix
    | ChangeDay String
    | ChangeMonth String
    | ChangeYear String
    | ChangeName String
    | NextDays
    | PrevDays
    | ChangePeriodType String
    | ChangeSelected String
    | Update
    | Delete


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ Storage.save (modelToSaveModel newModel), cmds ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , setTimeToNow
            )

        SetTimeNow time ->
            let
                datetime =
                    Maybe.withDefault (DateTime.fromPosix (Time.millisToPosix 0)) (DateCalc.posixToDate time)
            in
            ( { model | time = datetime }
            , Cmd.none
            )

        ChangeDay day ->
            let
                asDayInForm form d =
                    { form | day = d }
            in
            ( { model | form = asDayInForm model.form day }
            , Cmd.none
            )

        ChangeMonth month ->
            let
                asMonthInForm form m =
                    { form | month = m }
            in
            ( { model | form = asMonthInForm model.form month }
            , Cmd.none
            )

        ChangeYear year ->
            let
                asYearInForm form y =
                    { form | year = y }
            in
            ( { model | form = asYearInForm model.form year }
            , Cmd.none
            )

        ChangeName name ->
            let
                asNameInForm form n =
                    { form | name = n }
            in
            ( { model | form = asNameInForm model.form name }
            , Cmd.none
            )

        NextDays ->
            ( { model | time = DateCalc.incrementDate model.time 14 }
            , Cmd.none
            )

        PrevDays ->
            ( { model | time = DateCalc.decrementDate model.time 14 }
            , Cmd.none
            )

        ChangePeriodType periodType ->
            ( { model | periodCycle = Biorhythm.PeroidCycle.fromString periodType }
            , setTimeToNow
            )

        ChangeSelected selectedName ->
            let
                mPerson =
                    People.get model.people selectedName
            in
            ( case mPerson of
                Nothing ->
                    model

                Just person ->
                    { model | form = personToForm person }
            , Cmd.none
            )

        Update ->
            ( case validateForm model.form of
                Nothing ->
                    model

                Just birthdate ->
                    { model | people = People.update model.people (Person birthdate.day birthdate.month birthdate.year model.form.name)}
            --, Cmd.map (always ChangeSelected model.form.name) Cmd.none
            , Cmd.none
            )

        Delete ->
            ( { model | people = People.remove model.people model.form.name }
            , Cmd.none
            )


personToForm : Person -> Form
personToForm person =
    Form (String.fromInt person.day) (Month.toString person.month) (String.fromInt person.year) person.name



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ type_ "number", placeholder "day", value model.form.day, onInput ChangeDay ] []
            , select [ onInput ChangeMonth, value model.form.month ] (Month.months |> List.map monthToOption)
            , input [ type_ "number", placeholder "year", value model.form.year, onInput ChangeYear ] []
            , input [ type_ "text", placeholder "name", value model.form.name, onInput ChangeName ] []
            , button [ onClick Update ] [ text "Add/Update" ]
            , button [ onClick Delete ] [ text "Delete" ]
            ]
        , div [] [ peopleSelect model.people model.form ]
        , div []
            [ input [ type_ "radio", value "normal", checked (Biorhythm.PeroidCycle.toString model.periodCycle == "normal"), onInput ChangePeriodType ] []
            , span [] [ text "Normal" ]
            , input [ type_ "radio", value "accurate", checked (Biorhythm.PeroidCycle.toString model.periodCycle == "accurate"), onInput ChangePeriodType ] []
            , span [] [ text "Accurate" ]
            ]
        , div [] [ button [ onClick PrevDays ] [ text "Prev" ], button [ onClick NextDays ] [ text "Next" ] ]
        , case validate model.form of
            Err err ->
                h2 [] [ text err ]

            Ok birthdate ->
                div []
                    [ drawDateInfo model birthdate
                    , let
                        daysSinceBirth =
                            DateCalc.daysSinceBirth birthdate model.time

                        range =
                            List.range (daysSinceBirth - 7) (daysSinceBirth + 8)
                      in
                      div [] [ Biorhythm.Chart.view (range |> List.map (calcData model.periodCycle birthdate)) model.zone ]
                    ]
        ]


peopleSelect : List Person -> Form -> Html Msg
peopleSelect people selected =
    keyedSelect ChangeSelected (.name selected) (List.append  [(Tuple.pair "-1" "------")] (List.map (\p -> (p.name, p.name)) people))

keyedSelect : (String -> a) -> String -> List ( String, String ) -> Html a
keyedSelect message selectedValue kvs =
    let
        toOption ( k, v ) =
            ( k
            , option
                [ value k
                , selected (k == selectedValue)
                , disabled (k == "-1")
                ]
                [ text v ]
            )
    in
    Keyed.node "select"
        [ Html.Events.onInput message ]
        (List.map toOption kvs)

monthToOption : String -> Html Msg
monthToOption v =
    option [ value v ] [ text v ]


drawDateInfo model birthdate =
    div [ style "display" "none"]
        [ p [] [ text ("center time: " ++ DateCalc.dateTimeToString model.zone model.time) ]
        , p [] [ text ("birthday: " ++ DateCalc.dateTimeToString model.zone birthdate) ]
        , p [] [ text ("days: " ++ String.fromInt (DateCalc.daysSinceBirth birthdate model.time)) ]
        ]


validateForm : Form -> Maybe BirthDate
validateForm form =
    Maybe.map3
        (\d m y -> { day = d, month = m, year = y })
        (String.toInt form.day)
        (Month.fromString form.month)
        (String.toInt form.year)


validateBirthDate : Maybe BirthDate -> Result String DateTime
validateBirthDate maybeBirthDate =
    Result.fromMaybe "Cannot parse input" maybeBirthDate
        |> Result.andThen DateCalc.fromPartsToDate


validate : Form -> Result String DateTime
validate form =
    validateBirthDate (validateForm form)


calcData : PeriodCycle -> DateTime -> Int -> BiorhythmData
calcData periodCycle birthDate day =
    let
        bio =
            Biorhythm.calculate periodCycle day

        posixTime =
            Time.millisToPosix (DateTime.toMillis birthDate + (86400000 * day))
    in
    BiorhythmData day bio.physical bio.emotional bio.intellectual posixTime
