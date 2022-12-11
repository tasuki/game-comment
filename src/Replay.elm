module Replay exposing (..)

import Array
import GameRecord as G
import Html as H
import Html.Attributes as HA


type alias Replay =
    { record : G.Record
    , currentMove : Int
    , variation : List G.Play
    }


emptyReplay : G.Record -> Replay
emptyReplay record =
    { record = record
    , currentMove = 0
    , variation = []
    }


lastMove : Replay -> Maybe G.Move
lastMove replay =
    case List.head replay.variation of
        Just { player, move } ->
            Just move

        Nothing ->
            Array.fromList replay.record.moves
                |> Array.get (replay.currentMove - 1)
                |> Maybe.map .move


play : G.Coords -> Replay -> Replay
play coords replay =
    let
        move =
            { player = G.onMove replay.currentMove, move = G.Place coords }

        recordMove =
            List.drop replay.currentMove replay.record.moves |> List.head
    in
    if replay.variation == [] && recordMove == Just move then
        next replay

    else
        { replay
            | currentMove = replay.currentMove + 1
            , variation = move :: replay.variation
        }


next : Replay -> Replay
next replay =
    case replay.variation of
        move :: rest ->
            replay

        [] ->
            case List.drop replay.currentMove replay.record.moves |> List.head of
                Nothing ->
                    replay

                Just move ->
                    { replay
                        | currentMove = replay.currentMove + 1
                    }


prev : Replay -> Replay
prev replay =
    case replay.variation of
        [] ->
            case List.take replay.currentMove replay.record.moves |> List.reverse |> List.head of
                Nothing ->
                    replay

                Just move ->
                    { replay
                        | currentMove = replay.currentMove - 1
                    }

        move :: rest ->
            { replay
                | currentMove = replay.currentMove - 1
                , variation = rest
            }



-- VIEW


chars : List Char
chars =
    String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


viewMove : Maybe G.Move -> Int -> G.Play -> List (H.Html msg)
viewMove currentMove moveNum { player, move } =
    let
        char : Int -> Char
        char coord =
            List.drop (coord - 1) chars |> List.head |> Maybe.withDefault '.'

        moveStr =
            case move of
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

        highlight =
            if currentMove == Just move then
                "highlight "

            else
                ""
    in
    [ H.span [ HA.class (class ++ highlight) ]
        [ H.text <| (String.fromInt <| moveNum + 1) ++ "." ++ moveStr ]
    , H.text " "
    ]


view : Replay -> List (H.Html msg)
view replay =
    [ H.div [ HA.class "player-black" ] [ H.text replay.record.black ]
    , H.div [ HA.class "player-white" ] [ H.text replay.record.white ]
    , H.div [ HA.class "clear" ] []
    , H.div [ HA.class "replay" ] (List.indexedMap (viewMove (lastMove replay)) replay.record.moves |> List.concat)
    , H.div [ HA.class "replay" ]
        (List.indexedMap
            (\i -> viewMove (lastMove replay) (i + replay.currentMove - List.length replay.variation))
            (List.reverse replay.variation)
            |> List.concat
        )
    ]
