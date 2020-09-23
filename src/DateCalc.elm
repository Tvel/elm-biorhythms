module DateCalc exposing (..)

import Biorhythm exposing (DaysSinceBirth)
import Maybe
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

intToMonth: Int -> Month
intToMonth month =
    case month of
        1 -> Jan
        2 -> Feb
        3 -> Mar
        4 -> Apr
        5 -> May
        6 -> Jun
        7 -> Jul
        8 -> Aug
        9 -> Sep
        10 -> Oct
        11 -> Nov
        12 -> Dec
        _ -> Debug.todo "should not happen"

monthToString: Month -> String
monthToString month =
    case month of
        Jan -> "Jan"
        Feb -> "Feb"
        Mar -> "Mar"
        Apr -> "Apr"
        May -> "May"
        Jun -> "Jun"
        Jul -> "Jul"
        Aug -> "Aug"
        Sep -> "Sep"
        Oct -> "Oct"
        Nov -> "Nov"
        Dec -> "Dec"

stringToMonth: String -> Maybe Month
stringToMonth month =
    case month of
        "Jan" -> Just Jan
        "Feb" -> Just Feb
        "Mar" -> Just Mar
        "Apr" -> Just Apr
        "May" -> Just May
        "Jun" -> Just Jun
        "Jul" -> Just Jul
        "Aug" -> Just Aug
        "Sep" -> Just Sep
        "Oct" -> Just Oct
        "Nov" -> Just Nov
        "Dec" -> Just Dec
        _ -> Nothing

decoderStringToMonth: String -> D.Decoder Month
decoderStringToMonth month =
    case (stringToMonth month) of
        Just m -> D.succeed m
        Nothing -> D.fail ("Invalid month: " ++ month)

dateToString: Zone -> Posix -> String
dateToString zone time =
    let
        year   = String.fromInt (Time.toYear zone time)
        month = monthToString (Time.toMonth zone time)
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