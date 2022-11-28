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


nextPlay : Move -> Maybe Play -> Play
nextPlay move play =
    let
        player =
            case Maybe.map .player play of
                Nothing ->
                    Black

                Just White ->
                    Black

                Just Black ->
                    White
    in
    { player = player, move = move }


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
    , moves : List Play
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
