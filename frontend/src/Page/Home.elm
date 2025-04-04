module Page.Home exposing (..)

import Browser.Navigation as Nav
import GameHelpers as GH
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import LittleGolem as LG
import Page exposing (Page)
import Route
import Session exposing (Session)



-- MODEL


type alias Picker =
    { game : Maybe G.Game
    , size : Int
    , identifier : String
    }


type alias Model =
    { session : Session
    , picker : Picker
    , message : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , picker = { game = Nothing, size = 0, identifier = "" }
      , message = "Hi."
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = PickGame G.Game
    | PickBoardSize String
    | CreateBoard
    | EnterIdentifier String
    | FetchLG String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickGame game ->
            let
                picker =
                    { game = Just game
                    , size = G.defaultSize game
                    , identifier = model.picker.identifier
                    }
            in
            ( { model | picker = picker }, Cmd.none )

        PickBoardSize size ->
            let
                picker =
                    model.picker
            in
            ( { model | picker = { picker | size = Maybe.withDefault 3 (String.toInt size) } }
            , Cmd.none
            )

        CreateBoard ->
            case ( model.picker.game, model.picker.size ) of
                ( Just game, size ) ->
                    ( model, Nav.pushUrl model.session.navKey (Route.toUrl <| Route.EmptyGame game size) )

                ( Nothing, _ ) ->
                    ( model, Cmd.none )

        EnterIdentifier identifier ->
            let
                picker =
                    model.picker
            in
            ( { model | picker = { picker | identifier = identifier } }, Cmd.none )

        FetchLG identifier ->
            case LG.toGameId identifier of
                Ok gameId ->
                    ( model, Nav.pushUrl model.session.navKey (Route.toUrl <| Route.Game "lg" gameId) )

                Err error ->
                    ( { model
                        | message =
                            "Could not read game id from the text you entered: [ " ++ error ++ " ]"
                      }
                    , Cmd.none
                    )



-- VIEW


viewPicker : Picker -> List (H.Html Msg)
viewPicker picker =
    let
        gameClass : G.Game -> String
        gameClass game =
            if picker.game == Just game then
                "active"

            else
                ""

        gamePicker : G.Game -> H.Html Msg
        gamePicker game =
            H.button
                [ HA.class <| gameClass game, HE.onClick <| PickGame game ]
                [ H.text <| G.gameString game ]

        sizePickerAndCreateButton : List (H.Html Msg)
        sizePickerAndCreateButton =
            case picker.game of
                Just _ ->
                    [ H.input
                        [ HA.type_ "number"
                        , HA.min "5"
                        , HA.max "50"
                        , HA.value <| String.fromInt picker.size
                        , HE.onInput PickBoardSize
                        , GH.onEnter CreateBoard
                        ]
                        []
                    , H.button
                        [ HE.onClick CreateBoard ]
                        [ H.text "Create empty board" ]
                    ]

                Nothing ->
                    []
    in
    [ H.p [] [ H.text "Create an empty board for:" ]
    , H.div [] (List.map gamePicker G.games)
    , H.div [] sizePickerAndCreateButton
    , H.br [] []
    , H.h3 [] [ H.text "~~ OR ~~" ]
    , H.br [] []
    , H.p [] [ H.text "Load a game from LittleGolem:" ]
    , H.div []
        [ H.input
            [ HA.placeholder "LittleGolem game number or url"
            , HA.type_ "text"
            , HA.value <| picker.identifier
            , HA.class "lg-picker"
            , HA.autofocus True
            , HE.onInput EnterIdentifier
            , GH.onEnter <| FetchLG picker.identifier
            ]
            []
        ]
    , H.div []
        [ H.button
            [ HE.onClick <| FetchLG picker.identifier ]
            [ H.text "Load game" ]
        ]
    ]


view : Model -> Page Msg
view model =
    { title = "Game Comment - Load Game"
    , extraClass = "picker narrower"
    , content = viewPicker model.picker
    , sidebar = Page.sideHelp
    }
