module Main exposing (..)

import Browser
import GameRecord exposing (Coords, Game)
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
    = Void


type alias Model =
    { game : Game, size : Int }


empty =
    { game = GameRecord.TwixT, size = 24 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( empty, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Void ->
            ( model, Cmd.none )


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none


intsToStr : List Int -> String
intsToStr ints =
    List.map String.fromInt ints |> String.join " "


view : Model -> Browser.Document Msg
view model =
    { title = "Game Comment"
    , body =
        [ Svg.svg
            [ SA.viewBox (intsToStr [ -1, -1, model.size + 1, model.size + 1 ])
            , SA.width "800"
            , SA.height "800"
            ]
            (TwixT.view model.size)
        ]
    }
