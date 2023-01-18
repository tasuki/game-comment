module Main exposing (..)

import ApiClient as AC
import Browser
import Browser.Events
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import LittleGolem
import Replay as R
import Svg exposing (Svg)
import Svg.Attributes as SA
import TwixT


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { replay : Maybe R.Replay
    , message : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    -- ( empty, Cmd.none )
    ( empty, AC.getLittleGolemSgf Fetched "1107581" )


empty : Model
empty =
    { replay = Just testReplay, message = "You can use the left/right key to explore the game." }


testReplay : R.Replay
testReplay =
    let
        record : G.Record
        record =
            "(;FF[4]EV[twixt.ld.SIZE30]PB[tasuki]PW[David J Bush]SZ[30]SO[https://www.littlegolem.net]"
                ++ ";b[dc];r[op];b[qp];r[ol];b[sk];r[qt];b[us];r[tm];b[vq];r[rl];b[xl];r[wf];b[xh]"
                ++ ";r[ye];b[ug];r[vl];b[kl];r[qx];b[iq];r[it];b[wx];r[qj];b[mp];r[qq];b[oo];r[sp]"
                ++ ";b[ku];r[kw];b[ky];r[lu];b[mt];r[jy];b[nv];r[oy];b[resign])"
                |> LittleGolem.parse
                |> Result.withDefault G.empty

        smallRecord =
            "(;FF[4]EV[twixt]PB[black]PW[white]SZ[12];b[dc];r[bb];b[ee])" |> LittleGolem.parse |> Result.withDefault G.empty
    in
    R.emptyReplay record



-- UPDATE


type Msg
    = Noop
    | Play G.Coords
    | Forward
    | Backward
    | Jump R.LookAt
    | Fetch String
    | Fetched AC.SgfResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Backward ->
            ( { model | replay = Maybe.map R.prev model.replay }, Cmd.none )

        Forward ->
            ( { model | replay = Maybe.map R.next model.replay }, Cmd.none )

        Jump lookAt ->
            ( { model | replay = Maybe.map (R.jump lookAt) model.replay }, Cmd.none )

        Play coords ->
            ( { model | replay = Maybe.map (R.playCoords coords) model.replay }, Cmd.none )

        Fetch gameId ->
            ( model, AC.getLittleGolemSgf Fetched gameId )

        Fetched result ->
            case result of
                Ok record ->
                    ( { model | replay = Just <| R.emptyReplay record }, Cmd.none )

                Err error ->
                    ( { model | message = "Could not load game..." }, Cmd.none )



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


mainView : Maybe R.Replay -> List (H.Html Msg)
mainView maybeReplay =
    case maybeReplay of
        Just replay ->
            let
                intsToStr : List Int -> String
                intsToStr ints =
                    List.map String.fromInt ints |> String.join " "

                size =
                    replay.record.size

                specificView : R.Replay -> (G.Coords -> msg) -> List (Svg msg)
                specificView =
                    case replay.record.game of
                        G.TwixT ->
                            TwixT.view

                        _ ->
                            TwixT.view
            in
            [ Svg.svg
                [ SA.viewBox (intsToStr [ 0, 0, size + 1, size + 1 ]) ]
                (specificView replay Play)
            ]

        Nothing ->
            []


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
            [ H.div [ HA.class "pure-u-md-2-3", HA.class "grow" ] (mainView model.replay)
            , H.div [ HA.class "pure-u-md-1-3" ] (sideView model)
            ]
        ]
    }
