module Route exposing (..)

import GameRecord as G
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), (<?>), Parser, s)


type Route
    = Home
    | Help
    | EmptyGame G.Game Int
    | LittleGolemGame String


parser : Parser (Route -> Route) Route
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Help (s "help")
        , Parser.map EmptyGame (s "board" </> gameParser </> Parser.int)
        , Parser.map LittleGolemGame (s "game" </> s "lg" </> Parser.string)
        ]


toUrl : Route -> String
toUrl r =
    case r of
        Home ->
            Url.absolute [] []

        Help ->
            Url.absolute [ "help" ] []

        EmptyGame game size ->
            Url.absolute [ "board", gameToUrl game, String.fromInt size ] []

        LittleGolemGame id ->
            Url.absolute [ "game", "lg", id ] []



-- Utils


gameToUrl : G.Game -> String
gameToUrl game =
    case game of
        G.Go ->
            "go"

        G.ToroidGo ->
            "toroid"

        G.TwixT ->
            "twixt"

        G.Hex ->
            "hex"


gameParser : Parser (G.Game -> a) a
gameParser =
    Parser.custom "GAME"
        (\url ->
            case url of
                "go" ->
                    Just G.Go

                "toroid" ->
                    Just G.ToroidGo

                "twixt" ->
                    Just G.TwixT

                "hex" ->
                    Just G.Hex

                _ ->
                    Nothing
        )
