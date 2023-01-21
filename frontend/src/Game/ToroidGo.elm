module Game.ToroidGo exposing (..)

import Array exposing (Array)
import GameHelpers as GH
import GameRecord as G
import Html as H
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


type alias Move =
    ( G.Player, G.Coords )


type alias Stone =
    Maybe G.Player


type alias Stones =
    Array Stone


type alias Position =
    { size : Int
    , stones : Stones
    }


emptyPosition size =
    { size = size
    , stones = Array.initialize (size * size) (always Nothing)
    }


getStone : Int -> Int -> Int -> Stones -> Stone
getStone size x y stones =
    Array.get ((x - 1) + (y - 1) * size) stones |> Maybe.withDefault Nothing


setStone : Int -> Int -> Int -> Stone -> Stones -> Stones
setStone size x y stone stones =
    Array.set ((x - 1) + (y - 1) * size) stone stones


add : G.Move -> Position -> Position
add { player, play } position =
    case play of
        G.Place coords ->
            let
                -- TODO uh there might be some limitations, also stone-taking
                stones =
                    setStone
                        position.size
                        coords.x
                        coords.y
                        (Just player)
                        position.stones
            in
            { position | stones = stones }

        _ ->
            position


positionFromMoves : Int -> List G.Move -> Position
positionFromMoves boardSize moves =
    List.foldl add (emptyPosition boardSize) moves



-- View


background : Int -> List (Svg msg)
background size =
    [ Svg.rect
        [ SA.x "-2.5"
        , SA.y "-2.5"
        , SA.width <| String.fromInt <| size + 6
        , SA.height <| String.fromInt <| size + 6
        , SA.fill "#BBB"
        ]
        []
    , Svg.rect
        [ SA.x "0.5"
        , SA.y "0.5"
        , SA.width <| String.fromInt <| size
        , SA.height <| String.fromInt <| size
        , SA.fill "#EEE"
        ]
        []
    ]


drawLines : Int -> Int -> List (Svg msg)
drawLines min max =
    let
        ( lineMin, lineMax ) =
            ( toFloat min - 0.5, toFloat max + 0.5 )

        offsets =
            List.range min max |> List.map toFloat

        line x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 <| String.fromFloat x1
                , SA.y1 <| String.fromFloat y1
                , SA.x2 <| String.fromFloat x2
                , SA.y2 <| String.fromFloat y2
                , SA.stroke "#666"
                , SA.strokeWidth ".05"
                ]
                []

        horizontal =
            List.map (\offset -> line lineMin offset lineMax offset) offsets

        vertical =
            List.map (\offset -> line offset lineMin offset lineMax) offsets
    in
    horizontal ++ vertical


drawStones : Int -> Int -> Position -> Maybe G.Move -> List (Svg msg)
drawStones min max position lastMove =
    let
        showStone : G.Coords -> List (H.Attribute msg) -> G.Player -> Svg msg
        showStone coords class player =
            Svg.circle
                (class
                    ++ [ SA.cx <| String.fromInt coords.x
                       , SA.cy <| String.fromInt coords.y
                       , SA.r "0.48"
                       , SA.stroke "black"
                       , SA.strokeWidth "0.07"
                       , SA.fill <| G.color player
                       ]
                )
                []

        normaliseCoord : Int -> Int
        normaliseCoord coord =
            if coord < 1 then
                coord + position.size

            else if coord > position.size then
                coord - position.size

            else
                coord

        maybeShowStone : G.Coords -> Maybe (Svg msg)
        maybeShowStone coords =
            let
                ( normX, normY ) =
                    ( normaliseCoord coords.x, normaliseCoord coords.y )

                stone : Stone
                stone =
                    getStone position.size normX normY position.stones

                class : List (H.Attribute msg)
                class =
                    Maybe.map (\p -> GH.classesProps lastMove p { x = normX, y = normY }) stone
                        |> Maybe.withDefault []
            in
            Maybe.map (showStone coords class) stone
    in
    List.map maybeShowStone (GH.coordList min max)
        |> List.filterMap identity


view : R.Replay -> (G.Coords -> msg) -> Svg msg
view replay playMsg =
    let
        size =
            replay.record.size

        ( min, max ) =
            ( -2, size + 3 )

        position =
            positionFromMoves size (R.currentMoves replay)
    in
    Svg.svg
        [ SA.viewBox (GH.intsToStr [ -3, -3, size + 7, size + 7 ]), SA.class "go" ]
        (background size
            ++ drawLines min max
            ++ drawStones min max position (R.lastMove replay)
        )
