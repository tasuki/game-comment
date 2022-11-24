module GameRecord exposing (..)


type Game
    = Go
    | ToroidGo
    | TwixT
    | Hex


type Player
    = Black
    | White


type alias Coords =
    { x : Int
    , y : Int
    }


type Move
    = Place Coords
    | Swap
    | Resign


type alias Play =
    { player : Player
    , move : Move
    }


type alias Record =
    { black : String
    , white : String
    , game : Game
    , size : Int
    , result : String
    , moves : List Play
    }


empty : Record
empty =
    { black = ""
    , white = ""
    , game = Go
    , size = 19
    , result = ""
    , moves = []
    }
