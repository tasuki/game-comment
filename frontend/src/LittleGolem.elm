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


coordParser : Parser Int
coordParser =
    let
        decrease : Int -> Int
        decrease i =
            i - 96

        charToCoord : String -> Int
        charToCoord str =
            -- actually is one char as per chompIf _ -> True
            String.toList str
                |> List.head
                |> Maybe.withDefault 'a'
                |> Char.toCode
    in
    oneOf
        [ succeed decrease
            |. symbol "{{"
            |= int
            |. symbol "}}"
        , succeed (charToCoord >> decrease)
            |= (getChompedString <| chompIf (\_ -> True))
        ]


playParser : Parser Play
playParser =
    oneOf
        [ succeed Swap |. symbol "swap"
        , succeed Resign |. symbol "resign"
        , succeed (\x y -> Place { x = x, y = y })
            |= coordParser
            |= coordParser
            |. oneOf
                [ succeed identity |. symbol "|draw"
                , succeed identity
                ]
            |. end
        ]


parseMove : Player -> String -> Result String Move
parseMove player play =
    Parser.run playParser play
        |> Result.mapError showDeadEnds
        |> Result.map (\p -> { player = player, play = p })


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
    let
        deadEndsString =
            List.map showDeadEnd deadEnds |> String.join ", "
    in
    "Problem when parsing: [ " ++ deadEndsString ++ " ]"


parse : String -> Result String Record
parse input =
    Parser.run parser input
        |> Result.mapError showDeadEnds
        |> Result.andThen nodesToRecord


gameIdParser : Parser String
gameIdParser =
    oneOf
        [ succeed identity
            |= (getChompedString <| chompWhile Char.isDigit)
            |. end
        , succeed identity
            |. chompUntil "gid="
            |. symbol "gid="
            |= (getChompedString <| chompWhile Char.isDigit)
        ]


toGameId : String -> Result String String
toGameId identifier =
    Parser.run gameIdParser identifier |> Result.mapError showDeadEnds
