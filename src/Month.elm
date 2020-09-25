module Month exposing (..)

import Json.Decode as D
import Time exposing (Month(..))

type alias Month = Time.Month

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul", "Aug","Sep","Oct","Nov", "Dec"]

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
