module Game.Go exposing (..)

import Game.Shared.Go exposing (..)
import GameHelpers as GH
import GameRecord as G
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA


normaliseCoords : Normalise
normaliseCoords _ coords =
    coords


isOnBoard : Int -> G.Coords -> Bool
isOnBoard size coords =
    coords.x > 0 && coords.y > 0 && coords.x <= size && coords.y <= size


neighbors : Neighbors
neighbors position coords =
    List.filter (isOnBoard position.size)
        [ { coords | x = coords.x - 1 }
        , { coords | x = coords.x + 1 }
        , { coords | y = coords.y - 1 }
        , { coords | y = coords.y + 1 }
        ]



-- VIEW


view : R.Replay -> (G.Coords -> msg) -> Svg msg
view replay playMsg =
    let
        size =
            replay.record.size

        ( min, max ) =
            ( 1, size )

        position =
            List.foldl (add neighbors) (emptyPosition size) (R.currentMoves replay)
    in
    Svg.svg
        [ SA.viewBox (GH.intsToStr [ 0, 0, size + 1, size + 1 ])
        , SA.class "go"
        ]
        (background size
            ++ viewLines (toFloat min) (toFloat max) min max
            ++ viewStones
                neighbors
                normaliseCoords
                min
                max
                position
                (R.lastMove replay)
                (G.onMove replay.lookingAt.move G.Go)
                playMsg
        )
