module Game.Shared.Go exposing (..)

import Array exposing (Array)
import GameHelpers as GH
import GameRecord as G
import List.Extra
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


type alias Stone =
    Maybe G.Player


type alias Stones =
    Array Stone


type alias Position =
    { size : Int
    , stones : Stones
    }


type alias Neighbors =
    Position -> G.Coords -> List G.Coords


type alias Normalise =
    Position -> G.Coords -> G.Coords


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


positionToStones : Position -> List ( G.Coords, Stone )
positionToStones position =
    List.map (\c -> ( c, getStone c position )) (GH.coordList 1 position.size)


findGroupWithoutLiberties : Neighbors -> Position -> G.Player -> G.Coords -> List G.Coords
findGroupWithoutLiberties neighbors position player coords =
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
            neighbors position toExplore
                |> List.any (\c -> getStone c position == Nothing)

        ownNeighbors : G.Coords -> List G.Coords
        ownNeighbors toExplore =
            neighbors position toExplore
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


takeAll : Neighbors -> G.Player -> G.Coords -> Position -> Position
takeAll neighbors player coords pos =
    neighbors pos coords
        |> List.filter (\c -> getStone c pos == (Just <| G.otherPlayer player))
        |> List.concatMap (findGroupWithoutLiberties neighbors pos (G.otherPlayer player))
        |> List.foldl (\c -> setStone Nothing c) pos


maybePlay : Neighbors -> G.Player -> G.Coords -> Position -> Maybe Position
maybePlay neighbors player coords position =
    let
        positionAfterMove =
            setStone (Just player) coords position

        positionAfterTake =
            takeAll neighbors player coords positionAfterMove
    in
    if findGroupWithoutLiberties neighbors positionAfterTake player coords == [] then
        Just positionAfterTake

    else
        Nothing


add : Neighbors -> G.Move -> Position -> Position
add neighbors { player, play } position =
    case play of
        G.Place coords ->
            case maybePlay neighbors player coords position of
                Just newPos ->
                    newPos

                Nothing ->
                    position

        _ ->
            position



-- VIEW


background : Int -> List (Svg msg)
background size =
    [ Svg.rect
        [ SA.x "0.5"
        , SA.y "0.5"
        , SA.width <| String.fromInt <| size
        , SA.height <| String.fromInt <| size
        , SA.fill "#EEE"
        ]
        []
    ]


viewLines : Float -> Float -> Int -> Int -> List (Svg msg)
viewLines lineMin lineMax offsetMin offsetMax =
    let
        offsets =
            List.range offsetMin offsetMax |> List.map toFloat

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


viewStones :
    Neighbors
    -> Normalise
    -> Int
    -> Int
    -> Position
    -> Maybe G.Move
    -> G.Player
    -> (G.Coords -> msg)
    -> List (Svg msg)
viewStones neighbors normaliseCoords min max position lastMove onMove playMsg =
    let
        viewStone : G.Coords -> G.Coords -> G.Player -> Svg msg
        viewStone coords normCoords player =
            Svg.circle
                (GH.classesProps lastMove player normCoords
                    ++ [ SA.cx <| String.fromInt coords.x
                       , SA.cy <| String.fromInt coords.y
                       , SA.r "0.48"
                       , SA.stroke "black"
                       , SA.strokeWidth "0.07"
                       , SA.fill <| G.color player
                       ]
                )
                []

        viewEmpty : G.Coords -> G.Coords -> Svg msg
        viewEmpty coords normCoords =
            case maybePlay neighbors onMove coords position of
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

        showPosition : G.Coords -> Svg msg
        showPosition coords =
            let
                normCoords =
                    normaliseCoords position coords
            in
            case getStone normCoords position of
                Just player ->
                    viewStone coords normCoords player

                Nothing ->
                    viewEmpty coords normCoords
    in
    List.map showPosition (GH.coordList min max)
