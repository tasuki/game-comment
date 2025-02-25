module Route exposing (..)

import GameRecord as G
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, s)


type Route
    = Home
    | Login
    | LoggedIn String String
    | UserDetails String
    | Help
    | Game String String
    | EmptyGame G.Game Int


parse : Url -> Maybe Route
parse url =
    Parser.parse parser url


parser : Parser (Route -> Route) Route
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "user")
        , Parser.map LoggedIn (s "user" </> Parser.string </> Parser.string)
        , Parser.map UserDetails (s "user" </> Parser.string)
        , Parser.map Help (s "help")
        , Parser.map Game (s "game" </> Parser.string </> Parser.string)
        , Parser.map EmptyGame (s "board" </> gameParser </> Parser.int)
        ]


toUrl : Route -> String
toUrl r =
    case r of
        Home ->
            Builder.absolute [] []

        Login ->
            Builder.absolute [ "user" ] []

        LoggedIn name pass ->
            Builder.absolute [ "user", name, pass ] []

        UserDetails name ->
            Builder.absolute [ "user", name ] []

        Help ->
            Builder.absolute [ "help" ] []

        Game source id ->
            Builder.absolute [ "game", source, id ] []

        EmptyGame game size ->
            Builder.absolute [ "board", gameToUrl game, String.fromInt size ] []



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
