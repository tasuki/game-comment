module Replay.GameTree exposing (..)

import GameRecord as G
import Replay.Tree as T


type alias GameView =
    T.Zipper G.Move


treeToGameView : T.Tree G.Move -> GameView
treeToGameView =
    T.makeZipper


treeFromRecord : G.Record -> T.Tree G.Move
treeFromRecord record =
    T.listToLockedTree record.moves


addOrVisitChild : G.Move -> GameView -> GameView
addOrVisitChild move gameView =
    let
        ( index, newView ) =
            case T.findChildIndex move gameView of
                Just i ->
                    ( i, gameView )

                Nothing ->
                    T.addChild move gameView
    in
    T.lookNextByIndex index newView
        |> Maybe.withDefault gameView


withRecord : T.Tree G.Move -> GameView -> GameView
withRecord record zipper =
    -- TODO
    zipper
