module Month exposing (..)

import Json.Decode as D
import Time exposing (Month(..))

type alias Month = Time.Month

fromInt: Int -> Month
fromInt month =
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

toString: Month -> String
toString month =
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

fromString: String -> Maybe Month
fromString month =
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

decoderFromString: String -> D.Decoder Month
decoderFromString month =
    case (fromString month) of
        Just m -> D.succeed m
        Nothing -> D.fail ("Invalid month: " ++ month)
