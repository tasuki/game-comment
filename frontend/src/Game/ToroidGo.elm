module Game.ToroidGo exposing (..)

import Game.Shared.Go exposing (..)
import GameHelpers as GH
import GameRecord as G
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA


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


isLegal : R.Replay -> G.Coords -> Bool
isLegal replay move =
    isMoveLegal
        neighbors
        (G.onMove replay.lookingAt.move G.ToroidGo)
        move
        (positionFromReplay replay)


positionFromReplay : R.Replay -> Position
positionFromReplay replay =
    List.foldl (add neighbors) (emptyPosition replay.record.size) (R.currentMoves replay)



-- VIEW


contextBackground : Int -> List (Svg msg)
contextBackground size =
    [ Svg.rect
        [ SA.x <| String.fromFloat (0.5 - context)
        , SA.y <| String.fromFloat (0.5 - context)
        , SA.width <| String.fromInt <| size + (context * 2)
        , SA.height <| String.fromInt <| size + (context * 2)
        , SA.fill "#BBB"
        ]
        []
    ]


view : R.Replay -> (G.Coords -> msg) -> Svg msg
view replay playMsg =
    let
        size =
            replay.record.size

        ( min, max ) =
            ( 1 - context, size + context )
    in
    Svg.svg
        [ SA.viewBox
            (GH.intsToStr
                [ -context
                , -context
                , size + 1 + (2 * context)
                , size + 1 + (2 * context)
                ]
            )
        , SA.class "go"
        ]
        (contextBackground size
            ++ background size
            ++ viewLines (toFloat min - 0.5) (toFloat max + 0.5) min max
            ++ viewStones
                normaliseCoords
                min
                max
                (positionFromReplay replay)
                (R.lastMove replay)
                (G.onMove replay.lookingAt.move G.ToroidGo)
                playMsg
        )
