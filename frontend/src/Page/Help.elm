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
        [ H.p [] [ H.text "Use the left/right keys to explore the game." ]
        , H.p [] [ H.text "Use the up/down keys to go to the start/end of the game." ]
        ]
    , sidebar = Page.sideHelp
    }
