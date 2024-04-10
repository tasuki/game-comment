module Game.ToroidGo exposing (..)

import Game.Shared.Go exposing (..)
import GameHelpers as GH
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


background : Int -> List (Svg msg)
background size =
    let
        rect sz =
            Svg.rect
                [ SA.x "0.5"
                , SA.y "0.5"
                , SA.width <| String.fromInt <| sz
                , SA.height <| String.fromInt <| sz
                , SA.fill "#CA5"
                ]
                []
    in
    [ lazy rect size ]


contextBackground : String -> String -> List (Svg msg)
contextBackground from to =
    [ Svg.rect
        [ SA.x from, SA.y from, SA.width to, SA.height to, SA.fill "#875" ]
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
    in
    Svg.svg
        [ SA.viewBox <| String.join " " [ from, from, to, to ]
        , SA.class "go"
        ]
        (contextBackground from to
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
