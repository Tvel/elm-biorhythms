module Biorhythm.PeroidCycle exposing (..)

type alias PeriodCycle = { physical: Float, emotional: Float, intellectual: Float }

accurateCycle : PeriodCycle
accurateCycle = { physical = 23.688437, emotional = 28.426125, intellectual = 33.163812 }

normalCycle : PeriodCycle
normalCycle = { physical = 23, emotional = 28, intellectual = 33 }

fromString: String -> PeriodCycle
fromString periodString = case periodString of
    "normal" -> normalCycle
    "accurate" -> accurateCycle
    _ -> normalCycle

toString: PeriodCycle -> String
toString periodCycle =
    if periodCycle == normalCycle then
        "normal"
    else if periodCycle == accurateCycle then
        "accurate"
    else "NA"