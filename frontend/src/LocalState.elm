module LocalState exposing (LocalState, decode, encode)

import Codec exposing (Codec, Value, value)
import Dict exposing (Dict)
import GameRecord as G
import Replay as R
import Replay.Tree as T


type alias LocalState =
    { showFullMenu : Bool
    , authToken : Maybe String
    , replays : Dict String R.Replay
    , wipComments : Dict String String
    }


localStateCodec : Codec LocalState
localStateCodec =
    Codec.object LocalState
        |> Codec.field "showFullMenu" .showFullMenu Codec.bool
        |> Codec.field "authToken" .authToken (Codec.maybe Codec.string)
        |> Codec.field "replays" .replays (Codec.dict replayCodec)
        |> Codec.field "wipComments" .wipComments (Codec.dict Codec.string)
        |> Codec.buildObject


encode : LocalState -> Value
encode =
    Codec.encoder localStateCodec


decode : Value -> Result Codec.Error LocalState
decode =
    Codec.decodeValue localStateCodec



-- Replay codecs


replayCodec : Codec R.Replay
replayCodec =
    Codec.object R.Replay
        |> Codec.field "record" .record recordCodec
        |> Codec.field "gameTree" .gameTree gameViewCodec
        |> Codec.buildObject


gameViewCodec : Codec R.GameView
gameViewCodec =
    zipperCodec moveCodec



-- Tree codecs


zipperCodec : Codec a -> Codec (T.Zipper a)
zipperCodec meta =
    Codec.object T.Zipper
        |> Codec.field "focus" .focus (treeCodec meta)
        |> Codec.field "before" .before (Codec.list (treeCodec meta))
        |> Codec.field "after" .after (Codec.list (treeCodec meta))
        |> Codec.field "crumbs" .crumbs (Codec.list (crumbCodec meta))
        |> Codec.buildObject


crumbCodec : Codec a -> Codec (T.Crumb a)
crumbCodec meta =
    Codec.object T.Crumb
        |> Codec.field "value" .value (Codec.maybe meta)
        |> Codec.field "before" .before (Codec.list (treeCodec meta))
        |> Codec.field "after" .after (Codec.list (treeCodec meta))
        |> Codec.buildObject


treeNodeDataCodec : Codec a -> Codec (T.Node a)
treeNodeDataCodec meta =
    Codec.object T.Node
        |> Codec.field "value" .value (Codec.maybe meta)
        |> Codec.field "defaultChild" .defaultChild Codec.int
        |> Codec.field "children" .children (Codec.list (treeCodec meta))
        |> Codec.buildObject


treeCodec : Codec a -> Codec (T.Tree a)
treeCodec meta =
    let
        match locked tree value =
            case value of
                T.Locked ->
                    locked

                T.Tree x ->
                    tree x
    in
    Codec.custom match
        |> Codec.variant0 "locked" T.Locked
        |> Codec.variant1 "tree" T.Tree (Codec.lazy (\_ -> treeNodeDataCodec meta))
        |> Codec.buildCustom



-- Game codecs


recordCodec : Codec G.Record
recordCodec =
    Codec.object G.Record
        |> Codec.field "source" .source gameSourceCodec
        |> Codec.field "black" .black Codec.string
        |> Codec.field "white" .white Codec.string
        |> Codec.field "game" .game gameCodec
        |> Codec.field "size" .size Codec.int
        |> Codec.field "result" .result Codec.string
        |> Codec.field "moves" .moves (Codec.list moveCodec)
        |> Codec.buildObject


gameSourceCodec : Codec G.GameSource
gameSourceCodec =
    let
        match gameSource value =
            case value of
                G.GameSource source gameId ->
                    gameSource source gameId
    in
    Codec.custom match
        |> Codec.variant2 "gameSource" G.GameSource Codec.string Codec.string
        |> Codec.buildCustom


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
