module Page.Game exposing (..)

import ApiClient as AC
import Browser.Events
import Game.ToroidGo
import Game.TwixT
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Replay as R
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
      , message = "You can use the left/right key to explore the game."
      }
    , Cmd.none
    )


initLg : String -> Session -> ( Model, Cmd Msg )
initLg lgId session =
    ( { session = session
      , replay = Nothing
      , message = "You can use the left/right key to explore the game."
      }
    , AC.getLittleGolemSgf Fetched lgId
    )



-- UPDATE


type Msg
    = Noop
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

                        _ ->
                            \_ _ -> Svg.svg [] []
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


view : Model -> List (H.Html Msg)
view model =
    [ H.div [ HA.class "pure-g" ]
        [ H.div [ HA.class "pure-u-md-2-3", HA.class "grow" ] [ boardView model ]
        , H.div [ HA.class "pure-u-md-1-3" ] (sideView model)
        ]
    ]
