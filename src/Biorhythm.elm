module Biorhythm exposing (..)

type alias Biorhythm = { physical: Float, emotional: Float, intellectual: Float }
type alias PeriodCycle = { physical: Float, emotional: Float, intellectual: Float }
type alias DaysSinceBirth = Int

accurateCycle : PeriodCycle
accurateCycle = { physical = 23.688437, emotional = 28.426125, intellectual = 33.163812 }

normalCycle : PeriodCycle
normalCycle = { physical = 23, emotional = 28, intellectual = 33 }

calculateBiorhythm: PeriodCycle -> DaysSinceBirth -> Biorhythm
calculateBiorhythm cycle days =
    {
        physical = (sin (( (2*pi) / cycle.physical) * (toFloat days) )) * 100,
        emotional = (sin (( (2*pi) / cycle.emotional) * (toFloat days) )) * 100,
        intellectual = (sin (( (2*pi) / cycle.intellectual) * (toFloat days) )) * 100
    }