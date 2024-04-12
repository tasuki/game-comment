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
        [ H.p [] [ H.text "Use the left/right keys to explore the game." ]
        , H.br [] []
        , H.p [] [ H.text "You can also use the more complicated vim bindings:" ]
        , H.ul []
            [ H.li [] [ H.text "k/j for previous/next move" ]
            , H.li [] [ H.text "h/l for previous/next variation" ]
            , H.li [] [ H.text "g/G to go to the start/end of the game" ]
            ]
        ]
    , sidebar = Page.sideHelp
    }
