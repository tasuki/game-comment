module Main exposing (..)

import Array
import Browser
import GameRecord as R
import LittleGolem
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


type Msg
    = Play R.Coords


type Replay
    = TwixTReplay R.Record TwixT.Replay


replaySize : Replay -> Int
replaySize replay =
    case replay of
        TwixTReplay record _ ->
            record.size


testReplay : Replay
testReplay =
    let
        record : R.Record
        record =
            LittleGolem.parse "(;FF[4]EV[twixt.ld.SIZE30]PB[tasuki]PW[David J Bush]SZ[30]SO[https://www.littlegolem.net];b[dc];r[op];b[qp];r[ol];b[sk];r[qt];b[us];r[tm];b[vq];r[rl];b[xl];r[wf];b[xh];r[ye];b[ug];r[vl];b[kl];r[qx];b[iq];r[it];b[wx];r[qj];b[mp];r[qq];b[oo];r[sp];b[ku];r[kw];b[ky];r[lu];b[mt];r[jy];b[nv];r[oy];b[resign])"
                |> Result.withDefault R.empty
    in
    TwixTReplay record TwixT.emptyReplay


type alias Model =
    { replay : Replay }


empty : Model
empty =
    { replay = testReplay }


init : () -> ( Model, Cmd Msg )
init _ =
    ( empty, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play coords ->
            case model.replay of
                TwixTReplay record twixtReplay ->
                    ( { model | replay = TwixTReplay record <| TwixT.play coords twixtReplay }
                    , Cmd.none
                    )


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none


intsToStr : List Int -> String
intsToStr ints =
    List.map String.fromInt ints |> String.join " "


boardView : Replay -> List (Svg Msg)
boardView replay =
    case replay of
        TwixTReplay record twixtReplay ->
            TwixT.view record twixtReplay Play


view : Model -> Browser.Document Msg
view model =
    let
        size =
            replaySize model.replay
    in
    { title = "Game Comment"
    , body =
        [ Svg.svg
            [ SA.viewBox (intsToStr [ -1, -1, size + 1, size + 1 ])
            , SA.width "800"
            , SA.height "800"
            ]
            (boardView model.replay)
        ]
    }
