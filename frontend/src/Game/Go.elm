module Game.Go exposing (..)

import Game.Shared.Go exposing (..)
import GameHelpers as GH
import GameRecord as G
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy exposing (..)


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


isLegal : G.Coords -> R.Replay -> Bool
isLegal =
    isMoveLegal neighbors



-- VIEW


background : String -> Int -> List (Svg msg)
background colour size =
    let
        rect sz =
            Svg.rect
                [ SA.x "0"
                , SA.y "0"
                , SA.width <| String.fromInt <| sz + 1
                , SA.height <| String.fromInt <| sz + 1
                , SA.fill colour
                ]
                []
    in
    [ lazy rect size ]


view : R.Replay -> (G.Coords -> msg) -> Svg msg
view replay playMsg =
    let
        size =
            replay.record.size

        borderAdjust =
            0.2

        ( from, to ) =
            ( 0.5 - borderAdjust, toFloat size + 2 * borderAdjust )

        ( min, max ) =
            ( 1, size )
    in
    Svg.svg
        [ SA.viewBox (GH.floatsToStr [ from, from, to, to ])
        , SA.class "go"
        ]
        (background (R.currentColour replay) size
            ++ drawChildren (R.children replay)
            ++ viewLines (toFloat min) (toFloat max) min max
            ++ viewStars size
            ++ viewStones
                normaliseCoords
                min
                max
                (positionFromReplay neighbors replay)
                (R.lastPlayed replay)
                playMsg
        )
