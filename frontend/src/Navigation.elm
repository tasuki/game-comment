module Navigation exposing (getGamesTiles, getNavigationTiles)

import Dict exposing (Dict)
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Replay as R
import Route exposing (Route)
import SvgImages as SI
import Url exposing (Url)


type alias NavTile msg =
    { route : Route
    , full : List (H.Html msg)
    , short : List (H.Html msg)
    , first : List (H.Html msg)
    , msg : Maybe msg
    }


firstLetter : String -> Char
firstLetter str =
    case String.toList str of
        [] ->
            '?'

        first :: _ ->
            first


showTile : Bool -> Url -> NavTile msg -> List (H.Html msg)
showTile showFullMenu currentUrl navTile =
    let
        class =
            if Route.isOfRoute currentUrl navTile.route then
                "current"

            else
                ""

        text =
            if showFullMenu then
                navTile.full

            else if Route.isOfRoute currentUrl navTile.route then
                navTile.short

            else
                navTile.first

        showCloseLink : List (H.Html msg)
        showCloseLink =
            case navTile.msg of
                Just msg ->
                    [ H.button [ HA.class "close", HE.onClick msg ] [ SI.close Nothing Nothing ] ]

                Nothing ->
                    []

        showLink : List (H.Html msg)
        showLink =
            [ H.a [ HA.href <| Route.toUrl navTile.route ] text ]
    in
    [ H.div
        [ HA.class "menu-item "
        , HA.class class
        ]
        (showLink ++ showCloseLink)
    ]


cutToFirstWordOrTenChars : String -> String
cutToFirstWordOrTenChars str =
    let
        chars =
            String.toList str

        firstWord =
            List.Extra.takeWhile (\c -> c /= ' ') chars
    in
    if List.length firstWord <= 10 then
        firstWord |> String.fromList

    else
        List.take 10 chars |> String.fromList


gameNavTile : Bool -> (String -> msg) -> ( String, R.Replay ) -> NavTile msg
gameNavTile showFullMenu closeMsg ( url, replay ) =
    let
        fullText =
            [ H.text <| G.gameString replay.record.game
            , H.span [] [ H.text ": " ]
            , H.span [ HA.class "player black" ] [ H.text replay.record.black ]
            , H.span [] [ H.text " vs " ]
            , H.span [ HA.class "player white" ] [ H.text replay.record.white ]
            ]

        shortText =
            [ H.span
                [ HA.class "player black" ]
                [ H.text <| cutToFirstWordOrTenChars replay.record.black ]
            , H.span [] [ H.text " vs " ]
            , H.span
                [ HA.class "player white" ]
                [ H.text <| cutToFirstWordOrTenChars replay.record.white ]
            ]

        firstL =
            [ H.text <| String.fromChar <| firstLetter <| G.gameString replay.record.game
            ]

        gameCloseMsg =
            if showFullMenu then
                Just <| closeMsg url

            else
                Nothing

        route =
            -- suffer
            Route.fromPath url |> Maybe.withDefault Route.Home
    in
    NavTile route fullText shortText firstL gameCloseMsg


getGamesTiles : (String -> msg) -> Bool -> Dict String R.Replay -> List (NavTile msg)
getGamesTiles closeMsg showFullMenu replays =
    Dict.toList replays
        |> List.sortBy Tuple.first
        |> List.map (gameNavTile showFullMenu closeMsg)


firstTiles : List (NavTile msg)
firstTiles =
    [ NavTile Route.Login [ H.text "user" ] [ H.text "user" ] [ H.text "u" ] Nothing
    , NavTile Route.Home [ H.text "new" ] [ H.text "new" ] [ H.text "n" ] Nothing
    , NavTile Route.Help [ H.text "help" ] [ H.text "help" ] [ H.text "?" ] Nothing
    ]


getNavigationTiles : msg -> Bool -> Url -> List (NavTile msg) -> H.Html msg
getNavigationTiles toggleMenuMsg showFullMenu currentUrl gamesTiles =
    let
        toggleButton : H.Html msg
        toggleButton =
            H.div
                [ HA.class "menu-item" ]
                [ H.button [ HE.onClick toggleMenuMsg ] [ SI.bars ] ]

        tiles : List (H.Html msg)
        tiles =
            List.concatMap (showTile showFullMenu currentUrl) (firstTiles ++ gamesTiles)
    in
    H.div [] <| toggleButton :: tiles
