module Replay exposing (..)

import GameRecord as G


type alias Replay pos =
    { record : G.Record
    , moves : List G.Play
    , currentMove : Int
    , currentPosition : pos
    , variationFromMove : Maybe Int
    }


emptyReplay : G.Record -> pos -> Replay pos
emptyReplay record position =
    { record = record
    , moves = []
    , currentMove = 0
    , currentPosition = position
    , variationFromMove = Nothing
    }


play : G.Coords -> (G.Play -> pos -> pos) -> Replay pos -> Replay pos
play coords updateFun replay =
    let
        move =
            { player = G.onMove replay.moves, move = G.Place coords }
    in
    { replay
        | moves = move :: replay.moves
        , currentMove = replay.currentMove + 1
        , currentPosition = updateFun move replay.currentPosition
    }


next : (G.Play -> pos -> pos) -> Replay pos -> Replay pos
next updateFun replay =
    case List.drop replay.currentMove replay.record.moves |> List.head of
        Nothing ->
            replay

        Just p ->
            { replay
                | moves = p :: replay.moves
                , currentMove = replay.currentMove + 1
                , currentPosition = updateFun p replay.currentPosition
            }


prev : (G.Play -> pos -> pos) -> Replay pos -> Replay pos
prev updateFun replay =
    case List.head replay.moves of
        Nothing ->
            replay

        Just move ->
            { replay
                | moves = List.drop 1 replay.moves
                , currentMove = replay.currentMove - 1
                , currentPosition = updateFun move replay.currentPosition
            }
