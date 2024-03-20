module Page exposing (..)

import Browser
import Html as H
import Html.Attributes as HA


type alias Page msg =
    { title : String
    , extraClass : String
    , content : List (H.Html msg)
    , sidebar : List (H.Html msg)
    }


viewPage : (pageMsg -> globalMsg) -> Page pageMsg -> List (H.Html globalMsg) -> Browser.Document globalMsg
viewPage toMsg pageView navigation =
    let
        { title, extraClass, content, sidebar } =
            pageView

        listToMsg : List (H.Html pageMsg) -> List (H.Html globalMsg)
        listToMsg =
            List.map (H.map toMsg)
    in
    { title = title
    , body =
        [ H.div [ HA.class "pure-g", HA.class extraClass ] navigation
        , H.div [ HA.class "pure-g", HA.class extraClass ]
            [ H.div [ HA.class "content pure-u-1 pure-u-md-2-3" ] (listToMsg content)
            , H.div [ HA.class "sidebar pure-u-1 pure-u-md-1-3" ] (listToMsg sidebar)
            ]
        ]
    }


sideHelp : List (H.Html msg)
sideHelp =
    [ H.p []
        [ H.text "Hi there, this is a toy project of "
        , H.br [] []
        , H.a [ HA.href "https://tasuki.org/" ] [ H.text "Vít tasuki Brunner" ]
        , H.text "."
        ]
    , H.p [] [ H.text "It is very much a work in progress." ]
    , H.p []
        [ H.text "You can "
        , H.a [ HA.href "https://github.com/tasuki/game-comment" ] [ H.text "see the source code" ]
        , H.text "."
        ]
    ]
