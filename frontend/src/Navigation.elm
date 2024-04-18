module Navigation exposing (getNavigationTiles)

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


getNavigationTiles : Url -> Bool -> msg -> (String -> msg) -> Dict String R.Replay -> H.Html msg
getNavigationTiles currentUrl showFullMenu toggleMenuMsg closeMsg replays =
    let
        replayTileText record =
            if showFullMenu then
                G.recordName record

            else
                String.fromChar <| firstLetter <| G.recordName record

        replayCloseMsg url =
            if showFullMenu then
                Just <| closeMsg url

            else
                Nothing

        toNavTile : ( String, R.Replay ) -> NavTile msg
        toNavTile ( url, replay ) =
            NavTile url (H.text <| replayTileText replay.record) (replayCloseMsg url)

        replayUrls : List (NavTile msg)
        replayUrls =
            Dict.toList replays |> List.sortBy (\( url, _ ) -> url) |> List.map toNavTile

        firstTiles : List (NavTile msg)
        firstTiles =
            [ NavTile (Route.toUrl Route.Home) (H.text "new") Nothing
            , NavTile (Route.toUrl Route.Help) (H.text "help") Nothing
            ]

        toggleButton : H.Html msg
        toggleButton =
            H.div
                [ HA.class "menu-item" ]
                [ H.button [ HE.onClick toggleMenuMsg ] [ SI.bars ] ]

        tiles : List (H.Html msg)
        tiles =
            List.concatMap (showTile currentUrl) (firstTiles ++ replayUrls)
    in
    H.div [] <| toggleButton :: tiles
