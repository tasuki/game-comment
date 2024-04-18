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


list : List (H.Html msg) -> H.Html msg
list elements =
    H.ul [] <| List.map (\el -> H.li [] [ el ]) elements


view : Model -> Page Msg
view _ =
    { title = "Help - Game Comment"
    , extraClass = "narrower"
    , content =
        [ H.p [] [ H.text "The UI is a work in progress and might change at any time." ]
        , H.br [] []
        , H.p [] [ H.text "Navigating the game:" ]
        , list
            [ H.text "Use the arrows on the sidebar to move forward/backward in the game."
            , H.text "Clicking anywhere on the board starts a variation from the current move."
            ]
        , H.br [] []
        , H.p [] [ H.text "There are some useful keyboard shortcuts:" ]
        , list
            [ H.text "Use the left/right keys to explore the game."
            , H.text "Up/down to switch between variations at the current move."
            ]
        , H.br [] []
        , H.p [] [ H.text "You can also use the more complicated vim bindings:" ]
        , list
            [ H.text "g/G to go to the start/end of the game"
            , H.text "h/l for previous/next move"
            , H.text "k/j for previous/next variation"
            , H.text "x to delete the current variation"
            ]
        ]
    , sidebar = Page.sideHelp
    }
