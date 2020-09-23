module BiorhythmViz exposing (BiorhythmData, view)

import Axis
import Color exposing (Color)
import Path exposing (Path)
import Scale exposing (ContinuousScale, OrdinalScale)
import Scale.Color
import Shape
import Statistics
import Time exposing (Zone)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes as Explicit exposing (class, dy, fill, fontFamily, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, strokeWidth, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em, percent)


w : Float
w = 900


h : Float
h = 450


padding : Float
padding = 60


series : List { label : String, accessor : BiorhythmData -> Float }
series =
    [ { label = "physical"
      , accessor = .physical
      }
    , { label = "emotional"
      , accessor = .emotional
      }
    , { label = "intellectual"
      , accessor = .intellectual
      }
    ]


accessors : List (BiorhythmData -> Float)
accessors =
    List.map .accessor series


values : BiorhythmData -> List Float
values i =
    List.map (\a -> identity <| a i) accessors


colorScale : OrdinalScale String Color
colorScale =
    List.map .label series
        |> Scale.ordinal Scale.Color.category10


color : String -> Color
color =
    Scale.convert colorScale >> Maybe.withDefault Color.black


view : List BiorhythmData -> Zone -> Svg msg
view model zone =
    let
        last =
            List.reverse model
                |> List.head
                |> Maybe.withDefault (BiorhythmData 0 0 0 0 (Time.millisToPosix 0))

        first =
            List.head model
                |> Maybe.withDefault (BiorhythmData 0 0 0 0 (Time.millisToPosix 0))

        --xScale : ContinuousScale Float
        --xScale =
        --    model
        --        |> List.map (.day >> toFloat)
        --        |> Statistics.extent
        --        |> Maybe.withDefault ( 0, 1 )
        --        |> Scale.linear ( 0, w - 2 * padding )
        xScale : ContinuousScale Time.Posix
        xScale =
            model
                |> List.map .time
                |> (\b -> ( List.head b |> Maybe.withDefault (Time.millisToPosix 0), List.reverse b |> List.head |> Maybe.withDefault (Time.millisToPosix 0) ))
                |> Scale.time Time.utc ( 0, w - 2 * padding )

        yScale : ContinuousScale Float
        yScale =
            model
                |> List.map (values >> List.maximum >> Maybe.withDefault -100)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\b -> ( -100, b ))
                |> Scale.linear ( h - 2 * padding, 0 )
                |> Scale.nice 4

        lineGenerator : ( Time.Posix, Float ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale (identity x), Scale.convert yScale (identity y) )

        line : (BiorhythmData -> Float) -> Path
        line accessor =
            List.map (\i -> ( .time i, accessor i )) model
                |> List.map lineGenerator
                |> Shape.line Shape.monotoneInXCurve

        xGridLine : Int -> Time.Posix -> Svg msg
        xGridLine index tick =
            TypedSvg.line
                [ y1 0
                , Explicit.y2 (percent 74)
                , x1 (Scale.convert xScale tick)
                , x2 (Scale.convert xScale tick)
                , stroke <| Paint Color.black
                , strokeWidth 1
                ]
                []
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ Axis.bottom [ Axis.tickCount 15 ] xScale ]

        -- bottom x ^
        -- mid X
        , g [ transform [ Translate (padding - 1) (h / 2) ] ]
            [ Axis.bottom [ Axis.tickCount 0 ] xScale ]

        -- top X
        , g [ transform [ Translate (padding - 1) (0 + padding) ] ]
            [ Axis.bottom [ Axis.tickCount 0 ] xScale ]

        -- days Y
        , g [ transform [ Translate (padding - 0.5) padding ] ] <| List.indexedMap xGridLine <| Scale.ticks xScale 15

        -- right Y
        , g [ transform [ Translate (w - padding - 1) padding ] ]
            [ Axis.left [ Axis.tickCount 0 ] yScale
            ]

        --left Y
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ Axis.left [ Axis.ticks (values first) ] yScale
            , text_ [ fontFamily [ "sans-serif" ], fontSize 10, x 5, y 5 ] [ text "" ]
            ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            (List.map
                (\{ accessor, label } ->
                    Path.element (line accessor)
                        [ stroke <| Paint <| color label
                        , strokeWidth 3
                        , fill PaintNone
                        ]
                )
                series
            )
        , g [ fontFamily [ "sans-serif" ], fontSize 10 ]
            (List.map
                (\{ accessor, label } ->
                    g
                        [ transform
                            [ Translate (w - padding + 10) (padding + Scale.convert yScale (identity (accessor last)))
                            ]
                        ]
                        [ text_ [ fill (Paint (color label)) ] [ text label ] ]
                )
                series
            )
        ]


type alias BiorhythmData =
    { day : Int
    , physical : Float
    , emotional : Float
    , intellectual : Float
    , time : Time.Posix
    }
