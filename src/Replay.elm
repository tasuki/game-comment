module Replay exposing (..)

import GameRecord as G


type alias Replay pos =
    { record : G.Record
    , currentMove : Int
    , currentPosition : pos
    }


emptyReplay : G.Record -> pos -> Replay pos
emptyReplay record position =
    { record = record
    , currentMove = 0
    , currentPosition = position
    }


play : G.Coords -> (G.Play -> pos -> pos) -> Replay pos -> Replay pos
play coords updateFun replay =
    let
        move =
            { player = G.onMove replay.currentMove, move = G.Place coords }
    in
    { replay
        | currentMove = replay.currentMove + 1
        , currentPosition = updateFun move replay.currentPosition
    }


next : (G.Play -> pos -> pos) -> Replay pos -> Replay pos
next updateFun replay =
    case List.drop replay.currentMove replay.record.moves |> List.head of
        Nothing ->
            replay

        Just p ->
            { replay
                | currentMove = replay.currentMove + 1
                , currentPosition = updateFun p replay.currentPosition
            }


prev : (G.Play -> pos -> pos) -> Replay pos -> Replay pos
prev updateFun replay =
    case List.take replay.currentMove replay.record.moves |> List.reverse |> List.head of
        Nothing ->
            replay

        Just move ->
            { replay
                | currentMove = replay.currentMove - 1
                , currentPosition = updateFun move replay.currentPosition
            }
