module Page.User exposing (..)

import Page exposing (Page)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Page Msg
view model =
    { title = "Game Comment - User Account"
    , extraClass = "user narrower"
    , content = []
    , sidebar = Page.sideHelp
    }
