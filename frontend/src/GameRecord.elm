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


onMove : Int -> Game -> Player
onMove moveNum game =
    let
        firstPlayer =
            case game of
                TwixT ->
                    White

                _ ->
                    Black
    in
    if modBy 2 moveNum == 0 then
        firstPlayer

    else
        otherPlayer firstPlayer


otherPlayer : Player -> Player
otherPlayer player =
    case player of
        Black ->
            White

        White ->
            Black


color : Player -> String
color p =
    case p of
        Black ->
            "black"

        White ->
            "white"


games : List Game
games =
    [ TwixT, Hex, ToroidGo, Go ]


gameString : Game -> String
gameString g =
    case g of
        Go ->
            "Go"

        ToroidGo ->
            "Toroid Go"

        TwixT ->
            "TwixT"

        Hex ->
            "Hex"


defaultSize : Game -> Int
defaultSize g =
    case g of
        Go ->
            19

        ToroidGo ->
            11

        TwixT ->
            24

        Hex ->
            13


type alias Record =
    { black : String
    , white : String
    , game : Game
    , size : Int
    , result : String
    , moves : List Move
    }


empty : Game -> Int -> Record
empty game size =
    { black = "Black"
    , white = "White"
    , game = game
    , size = size
    , result = ""
    , moves = []
    }


coordsToComparable : Coords -> ( Int, Int )
coordsToComparable c =
    ( c.x, c.y )


coordsFromComparable : ( Int, Int ) -> Coords
coordsFromComparable ( x, y ) =
    { x = x, y = y }
