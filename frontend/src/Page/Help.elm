module Page.Help exposing (..)

import Html as H
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
        [ H.p [] [ H.text "Press j for previous move, k for next move." ]
        , H.p [] [ H.text "Use the h/l keys to go to the start/end of the game." ]
        ]
    , sidebar = Page.sideHelp
    }
