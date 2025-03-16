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
        [ H.div [ HA.id "body", HA.class extraClass ]
            [ H.div [ HA.id "header", HA.class "pure-g" ] navigation
            , H.div [ HA.id "main" ]
                [ H.div [ HA.id "content", HA.class "pure-u-1 pure-u-md-3-5 pure-u-xl-1-2" ] (listToMsg content)
                , H.div [ HA.id "sidebar", HA.class "pure-u-1 pure-u-md-2-5 pure-u-xl-1-2" ] (listToMsg sidebar)
                ]
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
        , H.text " and "
        , H.a [ HA.href "https://github.com/tasuki/game-comment/issues" ] [ H.text "submit bug reports" ]
        , H.text "."
        ]
    ]
