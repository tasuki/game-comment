module Replay exposing (..)

import Array as A exposing (Array)
import GameRecord as G
import Html as H
import Html.Attributes as HA
import List.Extra


type alias Replay =
    { record : G.Record
    , currentMove : Int
    , inVariation : Bool
    , variation : Variation
    }


type alias Variation =
    { fromMove : Int
    , moves : Array G.Move
    }


emptyReplay : G.Record -> Replay
emptyReplay record =
    { record = record
    , currentMove = 0
    , inVariation = False
    , variation = emptyVariation
    }


emptyVariation : Variation
emptyVariation =
    { fromMove = 0, moves = A.empty }


allMoves : Replay -> List G.Move
allMoves replay =
    let
        fromMain =
            List.take replay.variation.fromMove replay.record.moves

        fromVar =
            A.toList replay.variation.moves
    in
    if replay.inVariation then
        List.append fromMain fromVar

    else
        replay.record.moves


currentMoves : Replay -> List G.Move
currentMoves replay =
    allMoves replay |> List.take replay.currentMove


lastMove : Replay -> Maybe G.Move
lastMove replay =
    allMoves replay |> List.take replay.currentMove |> List.Extra.last


nextMove : Replay -> Maybe G.Move
nextMove replay =
    allMoves replay |> List.drop replay.currentMove |> List.head


addMoveToVar : G.Move -> Replay -> Replay
addMoveToVar move replay =
    let
        var =
            if replay.inVariation then
                replay.variation

            else
                { emptyVariation | fromMove = replay.currentMove }

        addMove : G.Move -> Variation -> Variation
        addMove m variation =
            { variation | moves = A.push m variation.moves }
    in
    { replay
        | currentMove = replay.currentMove + 1
        , inVariation = True
        , variation = addMove move var
    }


playCoords : G.Coords -> Replay -> Replay
playCoords coords replay =
    let
        move : G.Move
        move =
            { player = G.onMove replay.currentMove, play = G.Place coords }
    in
    if Just move == nextMove replay then
        next replay

    else
        addMoveToVar move replay


next : Replay -> Replay
next replay =
    case nextMove replay of
        Nothing ->
            replay

        Just _ ->
            { replay | currentMove = replay.currentMove + 1 }


prev : Replay -> Replay
prev replay =
    case lastMove replay of
        Nothing ->
            replay

        Just _ ->
            if replay.inVariation && (replay.currentMove - 1) <= replay.variation.fromMove then
                { replay | currentMove = replay.currentMove - 1, inVariation = False }

            else
                { replay | currentMove = replay.currentMove - 1 }



-- VIEW


chars : List Char
chars =
    String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


viewMove : Bool -> Int -> G.Move -> List (H.Html msg)
viewMove highlight moveNum { player, play } =
    let
        char : Int -> Char
        char coord =
            List.drop (coord - 1) chars |> List.head |> Maybe.withDefault '.'

        moveStr =
            case play of
                G.Place coords ->
                    String.fromChar (char coords.x) ++ String.fromInt coords.y

                G.Swap ->
                    "Swap"

                G.Resign ->
                    "Resign"

        class =
            case player of
                G.Black ->
                    "player black "

                G.White ->
                    "player white "

        highlightClass =
            if highlight then
                "highlight "

            else
                ""
    in
    [ H.span [ HA.class (class ++ highlightClass) ]
        [ H.text <| String.fromInt moveNum ++ "." ++ moveStr ]
    , H.text " "
    ]


view : Replay -> List (H.Html msg)
view replay =
    let
        viewMoves indexFun highlightFun moves =
            List.indexedMap (\i -> viewMove (highlightFun (i + 1)) (indexFun (i + 1))) moves
                |> List.concat
    in
    [ H.div [ HA.class "player-black" ] [ H.text replay.record.black ]
    , H.div [ HA.class "player-white" ] [ H.text replay.record.white ]
    , H.div [ HA.class "clear" ] []
    , H.div [ HA.class "replay" ]
        (viewMoves
            identity
            (\i -> not replay.inVariation && replay.currentMove == i)
            replay.record.moves
        )
    , H.div [ HA.class "replay" ]
        (viewMoves
            (\i -> replay.variation.fromMove + i)
            (\i -> replay.inVariation && replay.currentMove == replay.variation.fromMove + i)
            (A.toList replay.variation.moves)
        )
    ]
