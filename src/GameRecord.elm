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


type Play
    = Place Coords
    | Swap
    | Resign


type alias Move =
    { player : Player
    , play : Play
    }


onMove : Int -> Player
onMove moveNum =
    if modBy 2 moveNum == 0 then
        White

    else
        Black


color : Player -> String
color p =
    case p of
        Black ->
            "black"

        White ->
            "white"


type alias Record =
    { black : String
    , white : String
    , game : Game
    , size : Int
    , result : String
    , moves : List Move
    }


empty : Record
empty =
    { black = ""
    , white = ""
    , game = TwixT
    , size = 19
    , result = ""
    , moves = []
    }
