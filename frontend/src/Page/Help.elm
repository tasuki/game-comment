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


view : Model -> Page Msg
view _ =
    { title = "Help - Game Comment"
    , extraClass = "limit-width"
    , content =
        [ H.p [] [ H.text "Use the left/right keys to explore the game." ]
        , H.p [] [ H.text "Use the up/down keys to go to the start/end of the game." ]
        ]
    , sidebar =
        [ H.p []
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
    }
