module LittleGolem exposing (..)

import GameRecord exposing (..)
import List.Extra
import Parser exposing (..)


type alias Property =
    ( String, String )


type alias Node =
    List Property


many : Parser a -> Parser (List a)
many item =
    sequence
        { item = item
        , start = ""
        , separator = ""
        , end = ""
        , spaces = succeed ()
        , trailing = Optional
        }


propertyParser : Parser Property
propertyParser =
    succeed Tuple.pair
        |= (getChompedString <| chompWhile Char.isAlpha)
        |. symbol "["
        |= (getChompedString <| chompUntil "]")
        |. symbol "]"


nodeParser : Parser Node
nodeParser =
    succeed identity
        |. symbol ";"
        |= many propertyParser


parser : Parser (List Node)
parser =
    succeed identity
        |. symbol "("
        |= many nodeParser
        |. symbol ")"
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

        charToCoord : String -> Parser Int
        charToCoord str =
            case String.uncons str of
                Just ( c, "" ) ->
                    succeed <| decrease <| Char.toCode <| c

                _ ->
                    problem "Expected exactly one character"
    in
    oneOf
        [ succeed decrease
            |. symbol "{{"
            |= int
            |. symbol "}}"
            |> backtrackable
        , getChompedString (chompIf (\_ -> True)) |> andThen charToCoord
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
    let
        nodeToStr : Node -> String
        nodeToStr nd =
            String.concat <| List.map (\( prop, val ) -> prop ++ ": " ++ val) nd
    in
    case ( find node blackMove, find node whiteMove ) of
        ( Just bp, Nothing ) ->
            parseMove Black bp

        ( Nothing, Just wp ) ->
            parseMove White wp

        ( Nothing, Nothing ) ->
            Err <| "A node with no moves: " ++ nodeToStr node

        _ ->
            Err <| "A node with too many moves: " ++ nodeToStr node


gameToRecord : GameSource -> Game -> Node -> List Node -> Result String Record
gameToRecord gameSource game first rest =
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
            { source = gameSource
            , black = doFind first blackName
            , white = doFind first whiteName
            , game = game
            , size = doFind first "SZ" |> String.toInt |> Maybe.withDefault 0
            , result = ""
            , moves = moves
            }
        )
        resultMoves


nodesToRecord : GameSource -> List Node -> Result String Record
nodesToRecord gameSource nodes =
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

            else if String.contains "toroid" ev || String.contains "torus" ev then
                Just ToroidGo

            else if String.contains "go" ev || String.contains "go" vr then
                Just Go

            else if String.contains "sunjang" vr then
                Just Go

            else
                Nothing
    in
    case ( maybeGame, List.tail nodes ) of
        ( Just game, Just rest ) ->
            gameToRecord gameSource game first rest

        ( Nothing, _ ) ->
            Err "We don't support this game (or perhaps could not determine the type of game record)"

        ( _, Nothing ) ->
            Err "This game seems to have no moves"


showDeadEnds : List DeadEnd -> String
showDeadEnds deadEnds =
    "Problem when parsing: [ " ++ Parser.deadEndsToString deadEnds ++ " ]"


parse : GameSource -> String -> Result String Record
parse gameSource input =
    Parser.run parser input
        |> Result.mapError showDeadEnds
        |> Result.andThen (nodesToRecord gameSource)


encodeCoord : Int -> String
encodeCoord n =
    let
        code =
            n + 96
    in
    if code < 127 then
        Char.fromCode code |> String.fromChar

    else
        "{{" ++ String.fromInt code ++ "}}"


encodePlay : Play -> String
encodePlay play =
    case play of
        Place coords ->
            encodeCoord coords.x ++ encodeCoord coords.y

        Swap ->
            "swap"

        Resign ->
            "resign"


escapeProperty : String -> String
escapeProperty value =
    value
        |> String.replace "\\" "\\\\"
        |> String.replace "]" "\\]"


property : String -> String -> String
property key value =
    key ++ "[" ++ escapeProperty value ++ "]"


recordToSgf : Record -> String
recordToSgf record =
    let
        ( ( blackName, whiteName ), ( blackMove, whiteMove ) ) =
            if record.game == TwixT then
                ( ( "PW", "PB" ), ( "r", "b" ) )

            else if record.game == Hex then
                ( ( "PB", "PW" ), ( "W", "B" ) )

            else
                ( ( "PB", "PW" ), ( "B", "W" ) )

        firstNode =
            ";"
                ++ property "EV" (gameString record.game)
                ++ property "SZ" (String.fromInt record.size)
                ++ property blackName record.black
                ++ property whiteName record.white

        moveNode move =
            let
                prop =
                    case move.player of
                        Black ->
                            blackMove

                        White ->
                            whiteMove
            in
            ";" ++ property prop (encodePlay move.play)
    in
    "(" ++ firstNode ++ String.concat (List.map moveNode record.moves) ++ ")"


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
