module Main exposing (..)

import ApiClient as AC
import Browser
import Browser.Events
import Game.ToroidGo
import Game.TwixT
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import LittleGolem as LG
import Replay as R
import Svg exposing (Svg)


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Picker =
    { game : Maybe G.Game
    , size : Int
    , identifier : String
    }


type alias Model =
    { replay : Maybe R.Replay
    , picker : Picker
    , message : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( empty, Cmd.none )


empty : Model
empty =
    { replay = Nothing
    , picker = { game = Nothing, size = 0, identifier = "" }
    , message = "You can use the left/right key to explore the game."
    }



-- UPDATE


type Msg
    = Noop
    | PickGame G.Game
    | PickBoardSize String
    | CreateBoard
    | EnterIdentifier String
    | Fetch String
    | Fetched AC.SgfResult
    | Play G.Coords
    | Forward
    | Backward
    | Jump R.LookAt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

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
            case model.picker.game of
                Just game ->
                    let
                        record =
                            G.empty game model.picker.size
                    in
                    ( { model | replay = Just <| R.emptyReplay record }, Cmd.none )

                Nothing ->
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
                    ( model, AC.getLittleGolemSgf Fetched gameId )

                Err error ->
                    ( { model
                        | message =
                            "Could not read game id from the text you entered: [ " ++ error ++ " ]"
                      }
                    , Cmd.none
                    )

        Fetched result ->
            case result of
                Ok record ->
                    ( { model
                        | replay = Just <| R.emptyReplay record
                        , message = empty.message
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | message = "Could not load game: [ " ++ error ++ " ]" }, Cmd.none )

        Backward ->
            ( { model | replay = Maybe.map R.prev model.replay }, Cmd.none )

        Forward ->
            ( { model | replay = Maybe.map R.next model.replay }, Cmd.none )

        Jump lookAt ->
            ( { model | replay = Maybe.map (R.jump lookAt) model.replay }, Cmd.none )

        Play coords ->
            ( { model | replay = Maybe.map (R.playCoords coords) model.replay }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map keydown <| D.field "key" D.string)
        ]


keydown : String -> Msg
keydown keycode =
    case keycode of
        "ArrowLeft" ->
            Backward

        "ArrowRight" ->
            Forward

        _ ->
            Noop



-- VIEW


mainView : Model -> H.Html Msg
mainView model =
    case model.replay of
        Just replay ->
            let
                specificView : R.Replay -> (G.Coords -> msg) -> Svg msg
                specificView =
                    case replay.record.game of
                        G.TwixT ->
                            Game.TwixT.view

                        G.ToroidGo ->
                            Game.ToroidGo.view

                        _ ->
                            \_ _ -> Svg.svg [] []
            in
            specificView replay Play

        Nothing ->
            H.div [ HA.class "picker" ] (viewPicker model.picker)


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


sideView : Model -> List (H.Html Msg)
sideView model =
    let
        prevNext =
            [ H.div []
                [ H.button [ HE.onClick Backward ] [ H.text "prev" ]
                , H.button [ HE.onClick Forward ] [ H.text "next" ]
                ]
            ]

        replayView : Maybe R.Replay -> List (H.Html Msg)
        replayView maybeReplay =
            case maybeReplay of
                Just replay ->
                    R.view Jump replay

                Nothing ->
                    []
    in
    [ H.div [ HA.class "game-info" ] (prevNext ++ replayView model.replay)
    , H.div [ HA.class "message" ] [ H.text model.message ]
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Game Comment"
    , body =
        [ H.div [ HA.class "pure-g" ]
            [ H.div [ HA.class "pure-u-md-2-3", HA.class "grow" ] [ mainView model ]
            , H.div [ HA.class "pure-u-md-1-3" ] (sideView model)
            ]
        ]
    }
