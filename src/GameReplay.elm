module GameReplay exposing (..)

import Array exposing (Array)
import GameRecord as R


type alias Stones =
    { blackStones : List R.Coords
    , whiteStones : List R.Coords
    }


type alias Replay pos =
    { record : R.Record
    , moves : Array R.Play
    , currentMove : Int
    , currentPosition : pos
    }
