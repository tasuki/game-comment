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



-- VIEW


list : List (H.Html msg) -> H.Html msg
list elements =
    H.ul [] <| List.map (\el -> H.li [] [ el ]) elements


view : Model -> Page Msg
view _ =
    { title = "Help - Game Comment"
    , extraClass = "narrower"
    , content =
        [ H.p [] [ H.text "The UI is a work in progress!" ]
        , H.p [] [ H.text "Navigating the game:" ]
        , list
            [ H.text "Use the arrows on the sidebar to move forward/backward in the game."
            , H.text "Click anywhere on the board to start a variation."
            , H.text "Click on a highlighted move in a comment to preview it."
            ]
        , H.p [] [ H.text "Keyboard shortcuts:" ]
        , list
            [ H.text "Use the left/right (or h/l) keys to see next/previous move."
            , H.text "Up/down to switch between the analysis mode and go through the comments."
            ]
        , H.p [] [ H.text "Keyboard shortcuts in analysis mode:" ]
        , list
            [ H.text "g/G to go to the start/end of the game"
            , H.text "k/j for previous/next variation"
            , H.text "x to delete the current variation"
            ]
        ]
    , sidebar = Page.sideHelp
    }
