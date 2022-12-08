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
        , currentPosition = updateFun move replay.currentPosition
    }
