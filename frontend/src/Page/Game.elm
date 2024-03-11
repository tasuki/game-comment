module Page.Game exposing (..)

import ApiClient as AC
import Browser.Events
import Browser.Navigation as Nav
import Game.Go
import Game.Hex
import Game.ToroidGo
import Game.TwixT
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Page exposing (Page)
import Replay as R
import Route
import Session exposing (Session)
import Svg exposing (Svg)



-- MODEL


type alias Model =
    { session : Session
    , replay : Maybe R.Replay
    , message : String
    }


initEmpty : G.Game -> Int -> Session -> ( Model, Cmd Msg )
initEmpty game size session =
    ( { session = session
      , replay = Just <| R.emptyReplay <| G.empty game size
      , message = "Use the left/right keys to explore the game."
      }
    , Cmd.none
    )


initLg : String -> Session -> ( Model, Cmd Msg )
initLg lgId session =
    ( { session = session
      , replay = Nothing
      , message = "Use the left/right keys to explore the game."
      }
    , AC.getLittleGolemSgf Fetched lgId
    )


initPrevious : R.Replay -> Session -> ( Model, Cmd Msg )
initPrevious replay session =
    ( { session = session
      , replay = Just replay
      , message = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Noop
    | Fetched AC.SgfResult
    | Play G.Coords
    | Forward
    | Backward
    | Start
    | End
    | Jump R.LookAt
    | New
    | Help


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Fetched result ->
            case result of
                Ok record ->
                    ( { model
                        | replay = Just <| R.emptyReplay record
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | message = "Could not load game: [ " ++ error ++ " ]" }, Cmd.none )

        Start ->
            ( { model | replay = Maybe.map R.start model.replay }, Cmd.none )

        Backward ->
            ( { model | replay = Maybe.map R.prev model.replay }, Cmd.none )

        Forward ->
            ( { model | replay = Maybe.map R.next model.replay }, Cmd.none )

        End ->
            ( { model | replay = Maybe.map R.end model.replay }, Cmd.none )

        Jump lookAt ->
            ( { model | replay = Maybe.map (R.jump lookAt) model.replay }, Cmd.none )

        Play coords ->
            let
                isMoveLegal replay =
                    case replay.record.game of
                        G.ToroidGo ->
                            Game.ToroidGo.isLegal replay coords

                        G.Go ->
                            Game.Go.isLegal replay coords

                        _ ->
                            True
            in
            case Maybe.map isMoveLegal model.replay of
                Just True ->
                    ( { model | replay = Maybe.map (R.playCoords coords) model.replay }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        New ->
            ( model, Nav.pushUrl model.session.navKey (Route.toUrl <| Route.Home) )

        Help ->
            ( model, Nav.pushUrl model.session.navKey (Route.toUrl <| Route.Help) )



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

        "ArrowUp" ->
            Start

        "ArrowDown" ->
            End

        _ ->
            Noop



-- VIEW


boardView : Model -> H.Html Msg
boardView model =
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

                        G.Go ->
                            Game.Go.view

                        G.Hex ->
                            Game.Hex.view
            in
            specificView replay Play

        Nothing ->
            H.div [] []


sideView : Model -> List (H.Html Msg)
sideView model =
    let
        prevNext =
            [ H.div []
                [ H.button [ HE.onClick Backward ] [ H.text "prev" ]
                , H.button [ HE.onClick Forward ] [ H.text "next" ]
                , H.button [ HA.class "new", HE.onClick New ] [ H.text "new" ]
                , H.button [ HA.class "new", HE.onClick Help ] [ H.text "help" ]
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


view : Model -> Page Msg
view model =
    let
        gameName =
            case model.replay of
                Just { record } ->
                    G.gameString record.game
                        ++ ": "
                        ++ record.black
                        ++ " vs "
                        ++ record.white

                Nothing ->
                    "Loading"

        extraClass =
            case model.replay of
                Just { record } ->
                    if record.game == G.Hex then
                        ""

                    else
                        "limit-width"

                Nothing ->
                    "limit-width"
    in
    { title = gameName ++ " - Game Comment"
    , body =
        [ H.div [ HA.class "pure-g", HA.class extraClass ]
            [ H.div [ HA.class "pure-u-1 pure-u-md-2-3" ] [ boardView model ]
            , H.div [ HA.class "pure-u-1 pure-u-md-1-3" ] (sideView model)
            ]
        ]
    }
