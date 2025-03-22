module Game.Shared.Go exposing (..)

import Array exposing (Array)
import GameHelpers as GH
import GameRecord as G
import List.Extra
import Maybe.Extra
import Page.Home exposing (Msg(..))
import Replay as R
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
            maybePlay neighbors player coords position
                |> Maybe.withDefault position

        _ ->
            position


positionFromReplay : Neighbors -> Int -> List G.Move -> Position
positionFromReplay neighbors boardSize currentMoves =
    List.foldl (add neighbors) (emptyPosition boardSize) currentMoves


isMoveLegal : Neighbors -> G.Coords -> R.Replay -> Bool
isMoveLegal neighbors move replay =
    Maybe.Extra.isJust <|
        maybePlay
            neighbors
            (R.onMove G.Go replay)
            move
            (positionFromReplay neighbors replay.record.size (R.currentMoves replay))



-- VIEW


drawChildren : List G.Coords -> List (Svg msg)
drawChildren children =
    let
        drawChild : G.Coords -> Svg msg
        drawChild coords =
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.2"
                , SA.fill "#0003"
                ]
                []
    in
    List.map drawChild children


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
                , SA.stroke "#0008"
                , SA.strokeWidth ".05"
                ]
                []

        horizontal =
            List.map (\offset -> lazy4 line lineMin offset lineMax offset) offsets

        vertical =
            List.map (\offset -> lazy4 line offset lineMin offset lineMax) offsets
    in
    horizontal ++ vertical


starPoints : Int -> List ( Int, Int )
starPoints boardSize =
    let
        half =
            (boardSize + 1) // 2
    in
    if modBy 2 boardSize == 0 then
        -- Fuck you, you deserve no star points.
        -- Be happy such transgressions
        -- against all that is right and just
        -- are even allowed around here.
        []

    else if boardSize >= 15 then
        [ ( 4, 4 ), ( half, half ), ( 4, half ) ]

    else if boardSize >= 13 then
        [ ( 4, 4 ), ( half, half ) ]

    else if boardSize >= 11 then
        [ ( 3, 3 ), ( half, half ) ]

    else if boardSize >= 5 then
        [ ( 3, 3 ) ]

    else
        []


orderedRemoveDupes : comparable -> List comparable -> List comparable
orderedRemoveDupes previous lst =
    case lst of
        x :: xs ->
            if x == previous then
                orderedRemoveDupes previous xs

            else
                x :: orderedRemoveDupes x xs

        [] ->
            []


allStarPoints : Int -> List ( Int, Int )
allStarPoints boardSize =
    starPoints boardSize
        |> List.map (\( x, y ) -> ( x - 1, y - 1 ))
        |> List.concatMap (\( x, y ) -> [ ( x, y ), ( y, x ) ])
        |> List.concatMap
            (\( x, y ) ->
                [ ( x, y )
                , ( boardSize - 1 - x, y )
                , ( x, boardSize - 1 - y )
                , ( boardSize - 1 - x, boardSize - 1 - y )
                ]
            )
        |> List.sort
        |> orderedRemoveDupes ( -42, -42 )


viewStarPoint : ( Int, Int ) -> Svg msg
viewStarPoint ( x, y ) =
    Svg.circle
        [ SA.cx <| String.fromInt (x + 1)
        , SA.cy <| String.fromInt (y + 1)
        , SA.r "0.1"
        , SA.fill "#0008"
        ]
        []


viewStars : Int -> List (Svg msg)
viewStars =
    allStarPoints >> List.map viewStarPoint


viewStones :
    Normalise
    -> Int
    -> Int
    -> Position
    -> Maybe G.Move
    -> (G.Coords -> msg)
    -> (G.Coords -> msg)
    -> List (Svg msg)
viewStones normaliseCoords min max position lastMove playMsg existingMsg =
    let
        highlightLastMove : G.Coords -> Svg msg
        highlightLastMove coords =
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.2"
                , SA.fill "#F33"
                ]
                []

        viewStone : G.Coords -> G.Player -> Svg msg
        viewStone coords player =
            Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.r "0.48"
                , SA.stroke "black"
                , SA.strokeWidth "0.06"
                , SA.fill <| G.color player
                , SE.onClick <| existingMsg coords
                ]
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
                    if GH.isLastMove lastMove normCoords then
                        Svg.g []
                            [ viewStone coords player
                            , highlightLastMove coords
                            ]

                    else
                        viewStone coords player

                Nothing ->
                    viewEmpty coords normCoords

        showKeyedPosition : G.Coords -> ( String, Svg msg )
        showKeyedPosition coords =
            ( "coords-" ++ String.fromInt coords.x ++ "-" ++ String.fromInt coords.y
            , lazy showPosition coords
            )
    in
    List.singleton <| Svg.Keyed.node "g" [] <| List.map showKeyedPosition (GH.coordList min max)


viewHighlight : Maybe G.Coords -> List (Svg msg)
viewHighlight maybeCoords =
    case maybeCoords of
        Just coords ->
            [ Svg.circle
                [ SA.cx <| String.fromInt coords.x
                , SA.cy <| String.fromInt coords.y
                , SA.fill <| "transparent"
                , SA.r <| "0.3"
                , SA.stroke "#F33"
                , SA.strokeWidth "0.12"
                ]
                []
            ]

        Nothing ->
            []
