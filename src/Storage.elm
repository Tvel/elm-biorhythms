port module Storage exposing (Person, SaveModel, decode, save)

import DateCalc exposing (decoderStringToMonth, monthToString)
import Json.Decode as D exposing (Error)
import Json.Encode as E
import Time exposing (Month)

port setStorage : E.Value -> Cmd msg

type alias Person =
    { day : Int, month : Month, year : Int, name : String }

type alias SaveModel =
    { periodCycle: String
    , people: List Person
    }

decode: E.Value -> Result Error SaveModel
decode data =
    D.decodeValue decoder data

save: SaveModel -> Cmd msg
save model =
    setStorage (encode  model)

encode : SaveModel -> E.Value
encode model =
  E.object
    [ ("periodCycle", E.string model.periodCycle)
    , ("people", E.list encodePerson model.people)
    ]

encodePerson: Person -> E.Value
encodePerson person =
    E.object
        [ ("day", E.int person.day)
        , ("month", E.string (monthToString person.month))
        , ("year", E.int person.day)
        , ("name", E.string person.name)
        ]

decoder : D.Decoder SaveModel
decoder =
  D.map2 SaveModel
    (D.field "periodCycle" D.string)
    (D.field "people" (D.list decoderPerson) )

decoderPerson : D.Decoder Person
decoderPerson =
    D.map4 Person
      (D.field "day" D.int)
      (D.field "month" (D.string |> D.andThen decoderStringToMonth))
      (D.field "year" D.int)
      (D.field "name" D.string)