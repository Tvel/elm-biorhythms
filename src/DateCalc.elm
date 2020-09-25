module DateCalc exposing (..)

import Biorhythm exposing (DaysSinceBirth)
import Maybe
import Month
import Result exposing (fromMaybe)
import Time exposing (..)
import DateTime exposing (..)
import Json.Decode as D

type alias BirthDate = {day: Int, month: Month, year: Int}

fromPartsToDate: BirthDate -> Result String DateTime
fromPartsToDate {day, month, year} =
    fromMaybe
        "Cannot parse birthday"
        (fromRawParts { day = day, month = month, year = year } { hours = 0, minutes = 0, seconds = 0, milliseconds = 0})

posixToDate: Time.Posix -> Maybe DateTime
posixToDate posix =
    Just (fromPosix posix)
    |> Maybe.andThen (setHours 0)
    |> Maybe.andThen (setMinutes 0)
    |> Maybe.andThen (setSeconds 0)
    |> Maybe.andThen (setSeconds 0)

daysSinceBirth: DateTime -> DateTime -> DaysSinceBirth
daysSinceBirth birthDate calcDate =
    getDayDiff birthDate calcDate


dateToString: Zone -> Posix -> String
dateToString zone time =
    let
        year   = String.fromInt (Time.toYear zone time)
        month = Month.toString (Time.toMonth zone time)
        day = String.fromInt (Time.toDay zone time)
    in
    day ++ "/" ++ month ++ "/" ++ year

dateTimeToString: Zone -> DateTime -> String
dateTimeToString zone time =
    dateToString zone (DateTime.toPosix time)

incrementDate date times =
    case times of
        0 -> DateTime.incrementDay date
        _ -> incrementDate (DateTime.incrementDay date) (times - 1)

decrementDate date times =
    case times of
        0 -> DateTime.decrementDay date
        _ -> decrementDate (DateTime.decrementDay date) (times - 1)