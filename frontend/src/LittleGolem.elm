module LittleGolem exposing (..)

import GameRecord exposing (..)
import List.Extra
import Parser exposing (..)


type alias Property =
    ( String, String )


type alias Node =
    List Property


propertyParser : Parser Property
propertyParser =
    succeed Tuple.pair
        |= (getChompedString <| chompUntil "[")
        |. symbol "["
        |= (getChompedString <| chompUntil "]")
        |. symbol "]"


nodeHelp : List Property -> Parser (Step (List Property) (List Property))
nodeHelp revProps =
    oneOf
        [ succeed (\_ -> Done (List.reverse revProps))
            |= oneOf [ symbol ";", symbol ")" ]
        , succeed (\p -> Loop (p :: revProps))
            |= propertyParser
        ]


nodeParser : Parser Node
nodeParser =
    succeed identity
        |= loop [] nodeHelp


parserHelp revNodes =
    oneOf
        [ succeed (\n -> Loop (n :: revNodes)) |= nodeParser
        , succeed () |> map (\_ -> Done (List.reverse revNodes))
        ]


parser : Parser (List Node)
parser =
    succeed identity
        |. symbol "("
        |. symbol ";"
        |= loop [] parserHelp
        |. end


find : Node -> String -> Maybe String
find node key =
    List.Extra.find (\( k, _ ) -> k == key) node
        |> Maybe.map Tuple.second


doFind : Node -> String -> String
doFind node key =
    find node key |> Maybe.withDefault ""


parseMove : Player -> String -> Result String Move
parseMove player move =
    let
        charToCoord char =
            Char.toCode char - 96
    in
    case String.toList move of
        [ col, row ] ->
            Ok { player = player, play = Place <| Coords (charToCoord col) (charToCoord row) }

        [ col, row, '|', 'd', 'r', 'a', 'w' ] ->
            Ok { player = player, play = Place <| Coords (charToCoord col) (charToCoord row) }

        [ 's', 'w', 'a', 'p' ] ->
            Ok { player = player, play = Swap }

        [ 'r', 'e', 's', 'i', 'g', 'n' ] ->
            Ok { player = player, play = Resign }

        _ ->
            Err <| "Can't parse a move: " ++ move


getMove : String -> String -> Node -> Result String Move
getMove blackMove whiteMove node =
    case ( find node blackMove, find node whiteMove ) of
        ( Just bp, Nothing ) ->
            parseMove Black bp

        ( Nothing, Just wp ) ->
            parseMove White wp

        ( Nothing, Nothing ) ->
            Err <| "A node with no moves: " ++ Debug.toString node

        _ ->
            Err <| "A node with too many moves: " ++ Debug.toString node


gameToRecord : Game -> Node -> List Node -> Result String Record
gameToRecord game first rest =
    let
        ( ( blackName, whiteName ), ( blackMove, whiteMove ) ) =
            if game == TwixT then
                ( ( "PW", "PB" ), ( "r", "b" ) )

            else if game == Hex then
                ( ( "PB", "PW" ), ( "W", "B" ) )

            else
                ( ( "PB", "PW" ), ( "B", "W" ) )

        resultMoves : Result String (List Move)
        resultMoves =
            rest
                |> List.map (getMove blackMove whiteMove)
                |> List.foldr (Result.map2 (::)) (Ok [])
    in
    Result.map
        (\moves ->
            { black = doFind first blackName
            , white = doFind first whiteName
            , game = game
            , size = doFind first "SZ" |> String.toInt |> Maybe.withDefault 0
            , result = ""
            , moves = moves
            }
        )
        resultMoves


nodesToRecord : List Node -> Result String Record
nodesToRecord nodes =
    let
        first =
            List.head nodes |> Maybe.withDefault []

        ev =
            doFind first "EV" |> String.toLower

        vr =
            doFind first "VR" |> String.toLower

        maybeGame : Maybe Game
        maybeGame =
            if String.contains "twixt" ev then
                Just TwixT

            else if String.contains "hex" ev then
                Just Hex

            else if String.contains "toroid" ev then
                Just ToroidGo

            else if String.contains "go" vr then
                Just Go

            else
                Nothing
    in
    case ( maybeGame, List.tail nodes ) of
        ( Just game, Just rest ) ->
            gameToRecord game first rest

        ( Nothing, _ ) ->
            Err "We don't support this game (or perhaps could not determine the type of game record)"

        ( _, Nothing ) ->
            Err "This game seems to have no moves"


showDeadEnd : DeadEnd -> String
showDeadEnd de =
    "DeadEnd: " ++ Debug.toString de


showDeadEnds : List DeadEnd -> String
showDeadEnds deadEnds =
    "Problem when parsing SGF: [" ++ (List.map showDeadEnd deadEnds |> String.join ", ") ++ "]"


parse : String -> Result String Record
parse input =
    Parser.run parser input |> Result.mapError showDeadEnds |> Result.andThen nodesToRecord
