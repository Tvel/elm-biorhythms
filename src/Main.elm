module Main exposing (..)

import Biorhythm exposing (Biorhythm, PeriodCycle, accurateCycle, calculateBiorhythm, normalCycle, periodCycleToString, stringToCycle)
import BiorhythmViz exposing (BiorhythmData)
import Browser
import DateCalc exposing (BirthDate, dateTimeToString, decoderStringToMonth, fromPartsToDate, intToMonth, monthToString, stringToMonth)
import DateTime exposing (DateTime, fromPosix)
import Debug exposing (toString)
import Html exposing (Html, button, div, h2, h6, input, li, option, p, select, span, text, ul)
import Html.Attributes exposing (checked, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Maybe
import Result exposing (fromMaybe)
import Storage exposing (Person)
import Task
import Time exposing (Month(..), Posix, Zone, ZoneName(..))


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
    , selected : Maybe Person
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
    , selected = Just { day = 11, month = Jul, year = 1990, name = "Tosil" }
    , zone = Time.utc
    , time = fromPosix (Time.millisToPosix 0)
    , periodCycle = accurateCycle
    , people = [ { day = 11, month = Jul, year = 1990, name = "Tosil" } ]
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case Storage.decode flags of
        Ok saveModel ->
            { defaultModel | periodCycle = stringToCycle saveModel.periodCycle }

        Err _ ->
            defaultModel
    , Task.perform AdjustTimeZone Time.here
    )


modelToSaveModel : Model -> Storage.SaveModel
modelToSaveModel model =
    Storage.SaveModel (periodCycleToString model.periodCycle) model.people


setTimeToNow =
    Task.perform SetTimeNow Time.now


type Msg
    = AdjustTimeZone Time.Zone
    | SetTimeNow Time.Posix
    | ChangeDay String
    | ChangeMonth String
    | ChangeYear String
    | NextDays
    | PrevDays
    | ChangePeriodType String


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
                    Maybe.withDefault (fromPosix (Time.millisToPosix 0)) (DateCalc.posixToDate time)
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

        NextDays ->
            ( { model | time = DateCalc.incrementDate model.time 14 }
            , Cmd.none
            )

        PrevDays ->
            ( { model | time = DateCalc.decrementDate model.time 14 }
            , Cmd.none
            )

        ChangePeriodType periodType ->
            ( { model | periodCycle = stringToCycle periodType }
            , setTimeToNow
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ type_ "number", placeholder "day", value model.form.day, onInput ChangeDay ] []
            , select [ onInput ChangeMonth, value model.form.month ] (List.range 1 12 |> List.map intToMonth |> List.map monthToOption)
            , input [ type_ "number", placeholder "year", value model.form.year, onInput ChangeYear ] []
            ]
        , div []
            [ input [ type_ "radio", value "normal", checked (periodCycleToString model.periodCycle == "normal"), onInput ChangePeriodType ] []
            , span [] [ text "Normal" ]
            , input [ type_ "radio", value "accurate", checked (periodCycleToString model.periodCycle == "accurate"), onInput ChangePeriodType ] []
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
                      div [] [ BiorhythmViz.view (range |> List.map (calcData model.periodCycle birthdate)) model.zone ]
                    ]
        ]


monthToOption : Month -> Html Msg
monthToOption v =
    option [ value (toString v) ] [ text (toString v) ]


drawDateInfo model birthdate =
    div []
        [ p [] [ text ("center time: " ++ dateTimeToString model.zone model.time) ]
        , p [] [ text ("birthday: " ++ dateTimeToString model.zone birthdate) ]
        , p [] [ text ("days: " ++ String.fromInt (DateCalc.daysSinceBirth birthdate model.time)) ]
        ]


validateForm : Form -> Maybe BirthDate
validateForm form =
    Maybe.map3
        (\d m y -> { day = d, month = m, year = y })
        (String.toInt form.day)
        (stringToMonth form.month)
        (String.toInt form.year)


validateBirthDate : Maybe BirthDate -> Result String DateTime
validateBirthDate maybeBirthDate =
    fromMaybe "Cannot parse input" maybeBirthDate
        |> Result.andThen fromPartsToDate


validate : Form -> Result String DateTime
validate form =
    validateBirthDate (validateForm form)


calcData : PeriodCycle -> DateTime -> Int -> BiorhythmData
calcData periodCycle birthDate day =
    let
        bio =
            calculateBiorhythm periodCycle day

        posixTime =
            Time.millisToPosix (DateTime.toMillis birthDate + (86400000 * day))
    in
    BiorhythmData day bio.physical bio.emotional bio.intellectual posixTime



--biorhythmToUl : Biorhythm -> Html Msg
--biorhythmToUl biorhythm =
--    ul []
--        [ li [] [ text ("physical: " ++ String.fromFloat biorhythm.physical) ]
--        , li [] [ text ("emotional: " ++ String.fromFloat biorhythm.emotional) ]
--        , li [] [ text ("intellectual: " ++ String.fromFloat biorhythm.intellectual) ]
--        ]
--calcUl : Int -> Html Msg
--calcUl day =
--    biorhythmToUl (calculateBiorhythm accurateCycle day)
