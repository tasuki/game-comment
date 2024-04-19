module Navigation exposing (getGamesTiles, getNavigationTiles)

import Dict exposing (Dict)
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Replay as R
import Route
import SvgImages as SI
import Url exposing (Url)


type alias NavTile msg =
    { url : String
    , text : H.Html msg
    , msg : Maybe msg
    }


firstLetter : String -> Char
firstLetter str =
    case String.toList str of
        [] ->
            '?'

        first :: _ ->
            first


showTile : Url -> NavTile msg -> List (H.Html msg)
showTile currentUrl navTile =
    let
        getClass : String -> String
        getClass url =
            if url == currentUrl.path then
                "current"

            else
                ""

        showCloseLink : List (H.Html msg)
        showCloseLink =
            case navTile.msg of
                Just msg ->
                    [ H.button [ HA.class "close", HE.onClick msg ] [ SI.close Nothing Nothing ] ]

                Nothing ->
                    []

        showLink : List (H.Html msg)
        showLink =
            [ H.a [ HA.href navTile.url ] [ navTile.text ] ]
    in
    [ H.div
        [ HA.class "menu-item "
        , HA.class (getClass navTile.url)
        ]
        (showLink ++ showCloseLink)
    ]


gameNavTile : Bool -> (String -> msg) -> ( String, R.Replay ) -> NavTile msg
gameNavTile showFullMenu closeMsg ( url, replay ) =
    let
        gameTileText =
            if showFullMenu then
                G.recordName replay.record

            else
                String.fromChar <| firstLetter <| G.recordName replay.record

        gameCloseMsg =
            if showFullMenu then
                Just <| closeMsg url

            else
                Nothing
    in
    NavTile url (H.text <| gameTileText) gameCloseMsg


getGamesTiles : (String -> msg) -> Bool -> Dict String R.Replay -> List (NavTile msg)
getGamesTiles closeMsg showFullMenu replays =
    Dict.toList replays
        |> List.sortBy Tuple.first
        |> List.map (gameNavTile showFullMenu closeMsg)


firstTiles : List (NavTile msg)
firstTiles =
    [ NavTile (Route.toUrl Route.Home) (H.text "new") Nothing
    , NavTile (Route.toUrl Route.Help) (H.text "help") Nothing
    ]


getNavigationTiles : msg -> Url -> List (NavTile msg) -> H.Html msg
getNavigationTiles toggleMenuMsg currentUrl gamesTiles =
    let
        toggleButton : H.Html msg
        toggleButton =
            H.div
                [ HA.class "menu-item" ]
                [ H.button [ HE.onClick toggleMenuMsg ] [ SI.bars ] ]

        tiles : List (H.Html msg)
        tiles =
            List.concatMap (showTile currentUrl) (firstTiles ++ gamesTiles)
    in
    H.div [] <| toggleButton :: tiles
