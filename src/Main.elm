module Main exposing (..)

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
    { replay : GameReplay }


type GameReplay
    = TwixTReplay R.Replay


init : () -> ( Model, Cmd Msg )
init _ =
    ( empty, Cmd.none )


empty : Model
empty =
    { replay = TwixTReplay testReplay }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO this is horrid!
    -- not trivial to dedupe because of let-polymorphism?
    case msg of
        Noop ->
            ( model, Cmd.none )

        Backward ->
            case model.replay of
                TwixTReplay replay ->
                    ( { model | replay = TwixTReplay <| R.prev replay }, Cmd.none )

        Forward ->
            case model.replay of
                TwixTReplay replay ->
                    ( { model | replay = TwixTReplay <| R.next replay }, Cmd.none )

        Jump lookAt ->
            case model.replay of
                TwixTReplay replay ->
                    ( { model | replay = TwixTReplay <| R.jump lookAt replay }, Cmd.none )

        Play coords ->
            case model.replay of
                TwixTReplay replay ->
                    ( { model | replay = TwixTReplay <| R.playCoords coords replay }
                    , Cmd.none
                    )



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


getRecord : GameReplay -> G.Record
getRecord gr =
    case gr of
        TwixTReplay replay ->
            replay.record


boardView : GameReplay -> List (Svg Msg)
boardView gr =
    case gr of
        TwixTReplay replay ->
            TwixT.view replay Play


replayView : GameReplay -> List (H.Html Msg)
replayView gr =
    case gr of
        TwixTReplay replay ->
            R.view Jump replay


view : Model -> Browser.Document Msg
view model =
    let
        size =
            getRecord model.replay |> .size

        intsToStr : List Int -> String
        intsToStr ints =
            List.map String.fromInt ints |> String.join " "
    in
    { title = "Game Comment"
    , body =
        [ H.div [ HA.class "pure-g" ]
            [ Svg.svg
                [ SA.viewBox (intsToStr [ 0, 0, size + 1, size + 1 ])
                , SA.width "100%"
                , SA.class "pure-u-md-2-3"
                ]
                (boardView model.replay)
            , H.div
                [ HA.class "pure-u-md-1-3" ]
                [ H.div [ HA.class "game-info" ]
                    ([ H.div []
                        [ H.button [ HE.onClick Backward ] [ H.text "prev" ]
                        , H.button [ HE.onClick Forward ] [ H.text "next" ]
                        ]
                     ]
                        ++ replayView model.replay
                    )
                ]
            ]
        ]
    }
