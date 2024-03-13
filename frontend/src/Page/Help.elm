module Page.Help exposing (..)

import Html as H
import Html.Attributes as HA
import Page exposing (Page)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Noop


view : Model -> List (H.Html Msg) -> Page Msg
view _ nav =
    { title = "Help - Game Comment"
    , body =
        [ H.div [ HA.class "pure-g limit-width" ]
            [ H.div [ HA.class "pure-u-1 pure-u-md-2-3" ]
                [ H.p [] [ H.text "Use the left/right keys to explore the game." ]
                , H.p [] [ H.text "Use the up/down keys to go to the start/end of the game." ]
                ]
            , H.div [ HA.class "pure-u-1 pure-u-md-1-3" ]
                (nav
                    ++ [ H.p []
                            [ H.text "Hi there, this is a project of "
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
                )
            ]
        ]
    }
