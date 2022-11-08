module GameRecord exposing (..)


type Game
    = Go
    | ToroidGo
    | TwixT
    | Hex


type Player
    = Black
    | White


type alias Move =
    { player : Player
    , col : Int
    , row : Int
    }


type alias Record =
    { black : String
    , white : String
    , game : Game
    , result : String
    , moves : List Move
    }
