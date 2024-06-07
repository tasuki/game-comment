module Replay.GameTree exposing (..)

import GameRecord as G
import Replay.Tree as T


type alias GameView =
    T.Zipper G.Move


treesToGameView : T.Trees G.Move -> GameView
treesToGameView =
    T.makeZipper


treesFromRecord : G.Record -> List (T.Tree G.Move)
treesFromRecord record =
    let
        movesToTree : List a -> List (T.Tree a)
        movesToTree moves =
            case moves of
                head :: tail ->
                    [ T.Tree
                        { value = head
                        , defaultChild = 0
                        , children = movesToTree tail
                        }
                    ]

                [] ->
                    [ T.Locked ]
    in
    movesToTree record.moves


withRecord : T.Trees G.Move -> GameView -> GameView
withRecord record zipper =
    -- TODO
    zipper
