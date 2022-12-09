module Replay exposing (..)

import GameRecord as G


type alias Replay pos =
    { record : G.Record
    , currentMove : Int
    , variation : List G.Play
    , position : pos
    }


emptyReplay : G.Record -> pos -> Replay pos
emptyReplay record position =
    { record = record
    , currentMove = 0
    , variation = []
    , position = position
    }


play : G.Coords -> (G.Play -> pos -> pos) -> Replay pos -> Replay pos
play coords updateFun replay =
    let
        move =
            { player = G.onMove replay.currentMove, move = G.Place coords }

        recordMove =
            List.drop replay.currentMove replay.record.moves |> List.head
    in
    if replay.variation == [] && recordMove == Just move then
        next updateFun replay

    else
        { replay
            | currentMove = replay.currentMove + 1
            , variation = move :: replay.variation
            , position = updateFun move replay.position
        }


next : (G.Play -> pos -> pos) -> Replay pos -> Replay pos
next updateFun replay =
    case List.drop replay.currentMove replay.record.moves |> List.head of
        Nothing ->
            replay

        Just move ->
            { replay
                | currentMove = replay.currentMove + 1
                , position = updateFun move replay.position
            }


prev : (G.Play -> pos -> pos) -> Replay pos -> Replay pos
prev updateFun replay =
    case replay.variation of
        [] ->
            case List.take replay.currentMove replay.record.moves |> List.reverse |> List.head of
                Nothing ->
                    replay

                Just move ->
                    { replay
                        | currentMove = replay.currentMove - 1
                        , position = updateFun move replay.position
                    }

        move :: rest ->
            { replay
                | currentMove = replay.currentMove - 1
                , variation = rest
                , position = updateFun move replay.position
            }
