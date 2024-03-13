module Navigation exposing (..)

import Dict exposing (Dict)
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Replay as R
import Route
import Url exposing (Url)


addClass : a -> ( a, b ) -> ( a, b, String )
addClass currentUrl ( url, name ) =
    let
        class =
            if url == currentUrl then
                "current"

            else
                ""
    in
    ( url, name, class )


getNavigationTiles : Url -> Dict String R.Replay -> List (H.Html msg)
getNavigationTiles currentUrl replays =
    let
        replayUrls : List ( String, String )
        replayUrls =
            Dict.toList replays
                |> List.sortBy (\( url, _ ) -> url)
                |> List.map (\( url, replay ) -> ( url, G.recordName replay.record ))

        tiles : List ( String, String, String )
        tiles =
            ([ ( Route.toUrl Route.Home, "new" ) ] ++ replayUrls ++ [ ( Route.toUrl Route.Help, "help" ) ])
                |> List.map (addClass currentUrl.path)
    in
    List.map (\( url, recordName, isActive ) -> H.a [ HA.href url, HA.class "nav-item", HA.class isActive ] [ H.text recordName ]) tiles
