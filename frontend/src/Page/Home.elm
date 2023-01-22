module Page.Home exposing (..)

import Browser.Navigation as Nav
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import LittleGolem as LG
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
      , message = "You can use the left/right key to explore the game."
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = PickGame G.Game
    | PickBoardSize String
    | CreateBoard
    | EnterIdentifier String
    | Fetch String


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

        Fetch identifier ->
            case LG.toGameId identifier of
                Ok gameId ->
                    ( model, Nav.pushUrl model.session.navKey (Route.toUrl <| Route.LittleGolemGame gameId) )

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
                        ]
                        []
                    , H.button
                        [ HE.onClick CreateBoard ]
                        [ H.text "Create empty board" ]
                    ]

                Nothing ->
                    []
    in
    [ H.div [] (List.map gamePicker G.games)
    , H.div [] sizePickerAndCreateButton
    , H.hr [] []
    , H.div []
        [ H.input
            [ HA.placeholder "LittleGolem game number or url"
            , HA.type_ "text"
            , HA.size 40
            , HA.value <| picker.identifier
            , HE.onInput EnterIdentifier
            ]
            []
        ]
    , H.div []
        [ H.button
            [ HE.onClick <| Fetch picker.identifier ]
            [ H.text "Load game" ]
        ]
    ]


view : Model -> List (H.Html Msg)
view model =
    [ H.div [ HA.class "picker" ] (viewPicker model.picker) ]
