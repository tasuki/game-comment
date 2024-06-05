module LocalState exposing (LocalState, decode, encode)

import Codec exposing (Codec, Value)
import Dict exposing (Dict)
import GameRecord as G exposing (Game(..))
import Replay as R


type alias LocalState =
    { showFullMenu : Bool
    , replays : Dict String R.Replay
    }


localStateCodec : Codec LocalState
localStateCodec =
    Codec.object LocalState
        |> Codec.field "showFullMenu" .showFullMenu Codec.bool
        |> Codec.field "replays" .replays (Codec.dict replayCodec)
        |> Codec.buildObject


encode : LocalState -> Value
encode ls =
    Codec.encoder localStateCodec ls


decode : Value -> Result Codec.Error LocalState
decode val =
    Codec.decodeValue localStateCodec val



-- Replay codecs


replayCodec : Codec R.Replay
replayCodec =
    Codec.object R.Replay
        |> Codec.field "name" .name Codec.string
        |> Codec.field "record" .record recordCodec
        |> Codec.field "alterable" .alterable Codec.bool
        |> Codec.field "lookingAt" .lookingAt lookingAtCodec
        |> Codec.field "variations" .variations (Codec.list variationCodec)
        |> Codec.buildObject


lookingAtCodec : Codec R.LookAt
lookingAtCodec =
    Codec.object R.LookAt
        |> Codec.field "variation" .variation (Codec.maybe Codec.int)
        |> Codec.field "move" .move Codec.int
        |> Codec.buildObject


variationCodec : Codec R.Variation
variationCodec =
    Codec.object R.Variation
        |> Codec.field "colour" .colour Codec.string
        |> Codec.field "fromMove" .fromMove Codec.int
        |> Codec.field "moves" .moves (Codec.list moveCodec)
        |> Codec.buildObject



-- Game codecs


recordCodec : Codec G.Record
recordCodec =
    Codec.object G.Record
        |> Codec.field "black" .black Codec.string
        |> Codec.field "white" .white Codec.string
        |> Codec.field "game" .game gameCodec
        |> Codec.field "size" .size Codec.int
        |> Codec.field "result" .result Codec.string
        |> Codec.field "moves" .moves (Codec.list moveCodec)
        |> Codec.buildObject


gameCodec : Codec G.Game
gameCodec =
    Codec.custom
        (\go toroid twixt hex value ->
            case value of
                G.Go ->
                    go

                G.ToroidGo ->
                    toroid

                G.TwixT ->
                    twixt

                G.Hex ->
                    hex
        )
        |> Codec.variant0 "go" G.Go
        |> Codec.variant0 "toroid" G.ToroidGo
        |> Codec.variant0 "twixt" G.TwixT
        |> Codec.variant0 "hex" G.Hex
        |> Codec.buildCustom


moveCodec : Codec G.Move
moveCodec =
    Codec.object G.Move
        |> Codec.field "player" .player playerCodec
        |> Codec.field "play" .play playCodec
        |> Codec.buildObject


playerCodec : Codec G.Player
playerCodec =
    Codec.custom
        (\black white value ->
            case value of
                G.Black ->
                    black

                G.White ->
                    white
        )
        |> Codec.variant0 "black" G.Black
        |> Codec.variant0 "white" G.White
        |> Codec.buildCustom


playCodec : Codec G.Play
playCodec =
    Codec.custom
        (\place swap resign value ->
            case value of
                G.Place c ->
                    place c

                G.Swap ->
                    swap

                G.Resign ->
                    resign
        )
        |> Codec.variant1 "place" G.Place coordCodec
        |> Codec.variant0 "swap" G.Swap
        |> Codec.variant0 "resign" G.Resign
        |> Codec.buildCustom


coordCodec : Codec G.Coords
coordCodec =
    Codec.object G.Coords
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.buildObject
