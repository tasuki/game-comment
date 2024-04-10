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
    , extraClass = "narrower"
    , content =
        [ H.p [] [ H.text "Press k for previous move, j for next move." ]
        , H.p [] [ H.text "And g/G to go to the start/end of the game." ]
        ]
    , sidebar = Page.sideHelp
    }
