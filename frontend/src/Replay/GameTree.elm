module Replay.GameTree exposing (..)

import GameRecord as G
import Replay.Tree as T


type alias GameView =
    T.Zipper G.Move


type alias MoveTree =
    T.Tree G.Move


treeToGameView : MoveTree -> GameView
treeToGameView =
    T.makeZipper


treeFromRecord : G.Record -> MoveTree
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


currentMoves : GameView -> List G.Move
currentMoves =
    T.currentValues


currentMoveNumber : GameView -> Int
currentMoveNumber =
    currentMoves >> List.length


withRecord : List G.Move -> GameView -> GameView
withRecord record zipper =
    let
        replaced : T.Zipper G.Move
        replaced =
            T.makeTree zipper
                |> T.replaceFirstVar (Nothing :: List.map Just record)
                |> T.makeZipper

        goToMove : Int -> Maybe GameView -> Maybe GameView
        goToMove num mz =
            if num <= 0 then
                mz

            else
                goToMove (num - 1) (Maybe.andThen T.lookNext mz)
    in
    goToMove (currentMoveNumber zipper) (Just replaced)
        |> Maybe.withDefault zipper
