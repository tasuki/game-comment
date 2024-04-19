module Game.ToroidGo exposing (..)

import Game.Shared.Go exposing (..)
import GameRecord as G
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy exposing (..)


context =
    5


normaliseCoords : Normalise
normaliseCoords position coords =
    let
        normaliseCoord : Int -> Int
        normaliseCoord coord =
            if coord < 1 then
                coord + position.size

            else if coord > position.size then
                coord - position.size

            else
                coord
    in
    { x = normaliseCoord coords.x, y = normaliseCoord coords.y }


neighbors : Neighbors
neighbors position coords =
    List.map (normaliseCoords position)
        [ { coords | x = coords.x - 1 }
        , { coords | x = coords.x + 1 }
        , { coords | y = coords.y - 1 }
        , { coords | y = coords.y + 1 }
        ]


isLegal : G.Coords -> R.Replay -> Bool
isLegal =
    isMoveLegal neighbors



-- VIEW


background : String -> Int -> List (Svg msg)
background colour size =
    let
        rect sz =
            Svg.rect
                [ SA.x "0.5"
                , SA.y "0.5"
                , SA.width <| String.fromInt <| sz
                , SA.height <| String.fromInt <| sz
                , SA.fill colour
                ]
                []
    in
    [ lazy rect size ]


contextBackground : String -> String -> String -> List (Svg msg)
contextBackground fill from to =
    [ Svg.rect
        [ SA.x from, SA.y from, SA.width to, SA.height to, SA.fill fill ]
        []
    ]


view : R.Replay -> (G.Coords -> msg) -> Svg msg
view replay playMsg =
    let
        size =
            replay.record.size

        borderAdjust =
            0.05

        ( from, to ) =
            ( String.fromFloat <| 0.5 - context - borderAdjust
            , String.fromFloat <| toFloat size + (context * 2) + (borderAdjust * 2)
            )

        ( min, max ) =
            ( 1 - context, size + context )

        colour =
            R.currentColour replay
    in
    Svg.svg
        [ SA.viewBox <| String.join " " [ from, from, to, to ]
        , SA.class "go"
        ]
        (contextBackground colour from to
            ++ contextBackground "#3336" from to
            ++ background colour size
            ++ viewLines (toFloat min - 0.5) (toFloat max + 0.5) min max
            ++ viewStones
                normaliseCoords
                min
                max
                (positionFromReplay neighbors replay)
                (R.lastMove replay)
                playMsg
        )
