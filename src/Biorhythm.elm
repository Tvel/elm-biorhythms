module Biorhythm exposing (..)

import Biorhythm.PeroidCycle exposing (PeriodCycle)

type alias Biorhythm = { physical: Float, emotional: Float, intellectual: Float }
type alias DaysSinceBirth = Int

calculate: PeriodCycle -> DaysSinceBirth -> Biorhythm
calculate cycle days =
    {
        physical = (sin (( (2*pi) / cycle.physical) * (toFloat days) )) * 100,
        emotional = (sin (( (2*pi) / cycle.emotional) * (toFloat days) )) * 100,
        intellectual = (sin (( (2*pi) / cycle.intellectual) * (toFloat days) )) * 100
    }