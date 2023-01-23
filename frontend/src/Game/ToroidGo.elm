module Game.ToroidGo exposing (..)

import Array exposing (Array)
import GameHelpers as GH
import GameRecord as G
import Html as H
import List.Extra
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


context =
    5


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


emptyPosition : Int -> Position
emptyPosition size =
    { size = size
    , stones = Array.initialize (size * size) (always Nothing)
    }


getStone : G.Coords -> Position -> Stone
getStone coords position =
    Array.get ((coords.x - 1) + (coords.y - 1) * position.size) position.stones
        |> Maybe.withDefault Nothing


setStone : Stone -> G.Coords -> Position -> Position
setStone stone coords position =
    { position
        | stones =
            Array.set
                ((coords.x - 1) + (coords.y - 1) * position.size)
                stone
                position.stones
    }


allCoords : Position -> List G.Coords
allCoords position =
    let
        toSize =
            List.range 0 (position.size - 1)
    in
    List.concatMap (\x -> List.map (\y -> { x = x, y = y }) toSize) toSize


positionToStones : Position -> List ( G.Coords, Stone )
positionToStones position =
    List.map (\c -> ( c, getStone c position )) (allCoords position)


findGroupWithoutLiberties : Position -> G.Player -> G.Coords -> List G.Coords
findGroupWithoutLiberties position player coords =
    -- returns list of coords of the libertyless group, empty list if has liberties
    let
        empty =
            emptyPosition position.size

        coordsFromPosition : Position -> List G.Coords
        coordsFromPosition explored =
            positionToStones explored
                |> List.filter (\( _, s ) -> s == Just player)
                |> List.map (\( c, _ ) -> c)

        hasEmptyNeighbors : G.Coords -> Bool
        hasEmptyNeighbors toExplore =
            neighbors toExplore position
                |> List.any (\c -> getStone c position == Nothing)

        ownNeighbors : G.Coords -> List G.Coords
        ownNeighbors toExplore =
            neighbors toExplore position
                |> List.filter (\c -> getStone c position == Just player)

        isClosed : Position -> G.Coords -> Bool
        isClosed closed toExplore =
            case getStone toExplore closed of
                Just _ ->
                    True

                Nothing ->
                    False

        newOpenClosed : G.Coords -> List G.Coords -> Position -> ( List G.Coords, Position )
        newOpenClosed toExplore open closed =
            let
                toOpen =
                    ownNeighbors toExplore
                        |> List.Extra.filterNot (isClosed closed)

                newOpen =
                    toOpen ++ open

                newClosed =
                    List.foldl (\c -> setStone (Just player) c) closed toOpen
            in
            ( newOpen, newClosed )

        findLibertyless : ( List G.Coords, Position ) -> Position
        findLibertyless ( open, closed ) =
            case open of
                [] ->
                    -- libertyless
                    closed

                toExplore :: exploreLater ->
                    if hasEmptyNeighbors toExplore then
                        -- has at least one liberty
                        empty

                    else
                        -- explore further
                        findLibertyless <| newOpenClosed toExplore exploreLater closed
    in
    findLibertyless ( [ coords ], setStone (Just player) coords empty )
        |> coordsFromPosition


neighbors : G.Coords -> Position -> List G.Coords
neighbors coords pos =
    List.map (normaliseCoords pos)
        [ { coords | x = coords.x - 1 }
        , { coords | x = coords.x + 1 }
        , { coords | y = coords.y - 1 }
        , { coords | y = coords.y + 1 }
        ]


takeAll : G.Player -> G.Coords -> Position -> Position
takeAll player coords pos =
    neighbors coords pos
        |> List.filter (\c -> getStone c pos == (Just <| G.otherPlayer player))
        |> List.concatMap (findGroupWithoutLiberties pos (G.otherPlayer player))
        |> List.foldl (\c -> setStone Nothing c) pos


maybePlay : G.Player -> G.Coords -> Position -> Maybe Position
maybePlay player coords position =
    let
        positionAfterMove =
            setStone (Just player) coords position

        positionAfterTake =
            takeAll player coords positionAfterMove
    in
    if findGroupWithoutLiberties positionAfterTake player coords == [] then
        Just positionAfterTake

    else
        Nothing


add : G.Move -> Position -> Position
add { player, play } position =
    case play of
        G.Place coords ->
            case maybePlay player coords position of
                Just newPos ->
                    newPos

                Nothing ->
                    position

        _ ->
            position


positionFromMoves : Int -> List G.Move -> Position
positionFromMoves boardSize moves =
    List.foldl add (emptyPosition boardSize) moves


normaliseCoords : Position -> G.Coords -> G.Coords
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



-- View


background : Int -> List (Svg msg)
background size =
    [ Svg.rect
        [ SA.x <| String.fromFloat (0.5 - context)
        , SA.y <| String.fromFloat (0.5 - context)
        , SA.width <| String.fromInt <| size + (context * 2)
        , SA.height <| String.fromInt <| size + (context * 2)
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


drawStones : Int -> Int -> Position -> Maybe G.Move -> G.Player -> (G.Coords -> msg) -> List (Svg msg)
drawStones min max position lastMove onMove playMsg =
    let
        maybeShowStone : G.Coords -> Svg msg
        maybeShowStone coords =
            let
                normCoords =
                    normaliseCoords position coords

                stone : Stone
                stone =
                    getStone normCoords position

                class : List (H.Attribute msg)
                class =
                    Maybe.map (\p -> GH.classesProps lastMove p normCoords) stone
                        |> Maybe.withDefault []
            in
            case stone of
                Just player ->
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

                Nothing ->
                    case maybePlay onMove coords position of
                        Just _ ->
                            Svg.circle
                                [ SA.cx <| String.fromInt coords.x
                                , SA.cy <| String.fromInt coords.y
                                , SA.r "0.45"
                                , SA.fill "transparent"
                                , SE.onClick <| playMsg normCoords
                                ]
                                []

                        Nothing ->
                            Svg.svg [] []
    in
    List.map maybeShowStone (GH.coordList min max)


view : R.Replay -> (G.Coords -> msg) -> Svg msg
view replay playMsg =
    let
        size =
            replay.record.size

        ( min, max ) =
            ( 1 - context, size + context )

        position =
            positionFromMoves size (R.currentMoves replay)
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
        (background size
            ++ drawLines min max
            ++ drawStones
                min
                max
                position
                (R.lastMove replay)
                (G.onMove replay.lookingAt.move G.ToroidGo)
                playMsg
        )
