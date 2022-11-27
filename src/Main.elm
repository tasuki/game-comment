module Main exposing (..)

import Array
import Browser
import GameRecord as R
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
        e =
            R.empty
    in
    TwixTReplay
        { e | size = 24 }
        { moves = Array.empty
        , currentMove = 0
        , currentPosition =
            { pegs = [ ( R.Black, R.Coords 3 5 ), ( R.Black, R.Coords 4 4 ), ( R.White, R.Coords 4 5 ) ]
            , links = []
            }
        }


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
            -- TODO
            ( model, Cmd.none )


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none


intsToStr : List Int -> String
intsToStr ints =
    List.map String.fromInt ints |> String.join " "


boardView : Replay -> List (Svg msg)
boardView replay =
    case replay of
        TwixTReplay record twixtReplay ->
            TwixT.view record twixtReplay


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
