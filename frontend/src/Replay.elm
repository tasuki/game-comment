module Replay exposing (..)

import Array as A exposing (Array)
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra


type alias Replay =
    { record : G.Record
    , lookingAt : LookAt
    , variation : Variation
    }


type alias LookAt =
    { variation : Bool
    , move : Int
    }


type alias Variation =
    { fromMove : Int
    , moves : Array G.Move
    }


emptyReplay : G.Record -> Replay
emptyReplay record =
    { record = record
    , lookingAt = { variation = False, move = 0 }
    , variation = emptyVariation
    }


emptyVariation : Variation
emptyVariation =
    { fromMove = 0, moves = A.empty }


lookNext : LookAt -> LookAt
lookNext lookingAt =
    { lookingAt | move = lookingAt.move + 1 }


lookPrev : LookAt -> LookAt
lookPrev lookingAt =
    { lookingAt | move = lookingAt.move - 1 }



-- Moves from replay


allMoves : Replay -> List G.Move
allMoves replay =
    let
        fromMain =
            List.take replay.variation.fromMove replay.record.moves

        fromVar =
            A.toList replay.variation.moves
    in
    if replay.lookingAt.variation then
        List.append fromMain fromVar

    else
        replay.record.moves


currentMoves : Replay -> List G.Move
currentMoves replay =
    allMoves replay |> List.take replay.lookingAt.move


lastMove : Replay -> Maybe G.Move
lastMove replay =
    allMoves replay |> List.take replay.lookingAt.move |> List.Extra.last


nextMove : Replay -> Maybe G.Move
nextMove replay =
    allMoves replay |> List.drop replay.lookingAt.move |> List.head



-- Replay manipulations


addMoveToVar : G.Move -> Replay -> Replay
addMoveToVar move replay =
    let
        var =
            if replay.lookingAt.variation then
                let
                    variation =
                        replay.variation

                    till =
                        replay.lookingAt.move - variation.fromMove
                in
                { variation | moves = A.slice 0 till variation.moves }

            else
                { emptyVariation | fromMove = replay.lookingAt.move }

        addMove : G.Move -> Variation -> Variation
        addMove m variation =
            { variation | moves = A.push m variation.moves }
    in
    { replay
        | lookingAt = { variation = True, move = replay.lookingAt.move + 1 }
        , variation = addMove move var
    }


playCoords : G.Coords -> Replay -> Replay
playCoords coords replay =
    let
        move : G.Move
        move =
            { player = G.onMove replay.lookingAt.move replay.record.game, play = G.Place coords }
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
            { replay | lookingAt = lookNext replay.lookingAt }


prev : Replay -> Replay
prev replay =
    let
        lookAtPrev =
            lookPrev replay.lookingAt

        lookingEarlierThanVariation =
            (replay.lookingAt.move - 1) <= replay.variation.fromMove
    in
    case lastMove replay of
        Nothing ->
            replay

        Just _ ->
            if replay.lookingAt.variation && lookingEarlierThanVariation then
                { replay | lookingAt = { lookAtPrev | variation = False } }

            else
                { replay | lookingAt = lookAtPrev }


start : Replay -> Replay
start replay =
    let
        lookAt =
            replay.lookingAt
    in
    jump { lookAt | move = 0 } replay


end : Replay -> Replay
end replay =
    case nextMove replay of
        Nothing ->
            replay

        Just _ ->
            end { replay | lookingAt = lookNext replay.lookingAt }


jump : LookAt -> Replay -> Replay
jump lookAt replay =
    { replay | lookingAt = lookAt }



-- VIEW


chars : List Char
chars =
    String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


viewMoveHtml : msg -> Bool -> Int -> G.Move -> List (H.Html msg)
viewMoveHtml jumpMsg highlight moveNum { player, play } =
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
    [ H.button
        [ HE.onClick jumpMsg, HA.class (class ++ highlightClass) ]
        [ H.text <| String.fromInt moveNum ++ "." ++ moveStr ]
    ]


viewMove : (LookAt -> msg) -> Bool -> Replay -> Int -> G.Move -> List (H.Html msg)
viewMove jumpMsg inVar replay i =
    let
        moveNum =
            if inVar then
                replay.variation.fromMove + i

            else
                i

        highlight =
            if inVar then
                replay.lookingAt.variation
                    && (replay.lookingAt.move == replay.variation.fromMove + i)

            else
                not replay.lookingAt.variation
                    && (replay.lookingAt.move == i)
    in
    viewMoveHtml
        (jumpMsg { variation = inVar, move = moveNum })
        highlight
        moveNum


view : (LookAt -> msg) -> Replay -> List (H.Html msg)
view jumpMsg replay =
    let
        viewMoves : Bool -> List G.Move -> List (H.Html msg)
        viewMoves inVar moves =
            List.indexedMap (\i -> viewMove jumpMsg inVar replay (i + 1)) moves
                |> List.concat
    in
    [ H.div [ HA.class "player-black" ] [ H.text replay.record.black ]
    , H.div [ HA.class "player-white" ] [ H.text replay.record.white ]
    , H.div [ HA.class "clear" ] []
    , H.div [ HA.class "replay" ]
        (viewMoves False replay.record.moves)
    , H.div [ HA.class "replay" ]
        (viewMoves True (A.toList replay.variation.moves))
    ]
