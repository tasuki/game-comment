module Game.Shared.Go exposing (..)

import Array exposing (Array)
import GameHelpers as GH
import GameRecord as G
import List.Extra
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Svg.Keyed
import Svg.Lazy exposing (..)


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


coordsToArrayKey : Int -> G.Coords -> Int
coordsToArrayKey size coords =
    (coords.x - 1) + (coords.y - 1) * size


getStone : G.Coords -> Position -> Stone
getStone coords position =
    Array.get (coordsToArrayKey position.size coords) position.stones
        |> Maybe.withDefault Nothing


setStone : Stone -> G.Coords -> Position -> Position
setStone stone coords position =
    { position
        | stones =
            Array.set
                (coordsToArrayKey position.size coords)
                stone
                position.stones
    }


type alias Closed =
    Set ( Int, Int )


findGroupWithoutLiberties : Neighbors -> Position -> G.Player -> G.Coords -> List G.Coords
findGroupWithoutLiberties neighbors position player coords =
    -- returns list of coords of the libertyless group, empty list if has liberties
    let
        hasEmptyNeighbors : G.Coords -> Bool
        hasEmptyNeighbors toExplore =
            neighbors position toExplore
                |> List.any (\c -> getStone c position == Nothing)

        ownNeighbors : G.Coords -> List G.Coords
        ownNeighbors toExplore =
            neighbors position toExplore
                |> List.filter (\c -> getStone c position == Just player)

        isClosed : Closed -> G.Coords -> Bool
        isClosed closed toExplore =
            Set.member (G.coordsToComparable toExplore) closed

        newOpenClosed : G.Coords -> List G.Coords -> Closed -> ( List G.Coords, Closed )
        newOpenClosed toExplore open closed =
            let
                toOpen : List G.Coords
                toOpen =
                    ownNeighbors toExplore
                        |> List.Extra.filterNot (isClosed closed)

                newOpen =
                    toOpen ++ open

                newClosed =
                    List.foldl (G.coordsToComparable >> Set.insert) closed toOpen
            in
            ( newOpen, newClosed )

        findLibertyless : ( List G.Coords, Closed ) -> List G.Coords
        findLibertyless ( open, closed ) =
            case open of
                [] ->
                    -- libertyless
                    Set.toList closed |> List.map G.coordsFromComparable

                toExplore :: exploreLater ->
                    if hasEmptyNeighbors toExplore then
                        -- has at least one liberty
                        []

                    else
                        -- explore further
                        findLibertyless <| newOpenClosed toExplore exploreLater closed
    in
    findLibertyless ( [ coords ], Set.singleton <| G.coordsToComparable coords )


takeAll : Neighbors -> G.Player -> G.Coords -> Position -> Position
takeAll neighbors player coords pos =
    neighbors pos coords
        |> List.filter (\c -> getStone c pos == (Just <| G.otherPlayer player))
        |> List.concatMap (findGroupWithoutLiberties neighbors pos (G.otherPlayer player))
        |> List.foldl (\c -> setStone Nothing c) pos


maybePlay : Neighbors -> G.Player -> G.Coords -> Position -> Maybe Position
maybePlay neighbors player coords position =
    -- play the move if it's legal
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


isMoveLegal : Neighbors -> G.Player -> G.Coords -> Position -> Bool
isMoveLegal neighbors onMove move pos =
    case maybePlay neighbors onMove move pos of
        Just _ ->
            True

        Nothing ->
            False



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
                , SA.fill "#EEE"
                ]
                []
    in
    [ lazy rect size ]


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
            List.map (\offset -> lazy4 line lineMin offset lineMax offset) offsets

        vertical =
            List.map (\offset -> lazy4 line offset lineMin offset lineMax) offsets
    in
    horizontal ++ vertical


viewStones :
    Normalise
    -> Int
    -> Int
    -> Position
    -> Maybe G.Move
    -> G.Player
    -> (G.Coords -> msg)
    -> List (Svg msg)
viewStones normaliseCoords min max position lastMove onMove playMsg =
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
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.45"
                , SA.fill "transparent"
                , SE.onClick <| playMsg normCoords
                ]
                []

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

        showKeyedPosition : G.Coords -> ( String, Svg msg )
        showKeyedPosition coords =
            ( "coords-" ++ String.fromInt coords.x ++ "-" ++ String.fromInt coords.y
            , lazy showPosition coords
            )
    in
    List.singleton <| Svg.Keyed.node "g" [] <| List.map showKeyedPosition (GH.coordList min max)
