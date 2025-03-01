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


dummyUrl : Url
dummyUrl =
    { protocol = Url.Https
    , host = ""
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


fromPath : String -> Maybe Route
fromPath path =
    parse { dummyUrl | path = path }


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


isOfRoute : Url -> Route -> Bool
isOfRoute url route =
    -- is the route represented by the url?
    let
        contains r =
            case r of
                LoggedIn _ _ ->
                    route == Login

                other ->
                    route == other
    in
    case parse url of
        Just r ->
            contains r

        Nothing ->
            False
