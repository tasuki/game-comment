module Replay.TreeLayoutTests exposing (..)

import Expect
import Replay.Tree as T
import Replay.TreeLayout exposing (..)
import Replay.TreeTests as TT exposing (t)
import Test exposing (..)


empTree : T.Tree String
empTree =
    t "root" []


linTree : T.Tree String
linTree =
    t "root" [ t "A1" [ t "A2" [] ] ]


varTree : T.Tree String
varTree =
    t "root"
        [ t "A1" []
        , t "B1" []
        , t "C1" []
        ]



-- TT.trickyTree:
--
-- A1 A2 A3
--        \ E4
--        \ F4 F5 F6
--        \ G4
-- B1 B2 B3
--  \ C2 C3 C4


trickyTreeBranchesWithParents : List (BranchWithParents String)
trickyTreeBranchesWithParents =
    [ { nodes = [ "root", "A1", "A2", "A3" ], parents = [] }
    , { nodes = [ "E4" ], parents = [ "root", "A1", "A2", "A3" ] }
    , { nodes = [ "F4", "F5", "F6" ], parents = [ "root", "A1", "A2", "A3" ] }
    , { nodes = [ "G4" ], parents = [ "root", "A1", "A2", "A3" ] }
    , { nodes = [ "B1", "B2", "B3" ], parents = [ "root" ] }
    , { nodes = [ "C2", "C3", "C4" ], parents = [ "root", "B1" ] }
    ]


trickyTreeBranches : List (Branch String)
trickyTreeBranches =
    [ { parentBranch = 0, firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3" ] }
    , { parentBranch = 0, firstNodeNum = 4, nodes = [ "E4" ] }
    , { parentBranch = 0, firstNodeNum = 4, nodes = [ "F4", "F5", "F6" ] }
    , { parentBranch = 0, firstNodeNum = 4, nodes = [ "G4" ] }
    , { parentBranch = 0, firstNodeNum = 1, nodes = [ "B1", "B2", "B3" ] }
    , { parentBranch = 4, firstNodeNum = 2, nodes = [ "C2", "C3", "C4" ] }
    ]


trickyTreePositioned : List (PositionedBranch String)
trickyTreePositioned =
    [ { branchOffset = 0, firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3" ] }
    , { branchOffset = 1, firstNodeNum = 4, nodes = [ "E4" ] }
    , { branchOffset = 2, firstNodeNum = 4, nodes = [ "F4", "F5", "F6" ] }
    , { branchOffset = 3, firstNodeNum = 4, nodes = [ "G4" ] }
    , { branchOffset = 4, firstNodeNum = 1, nodes = [ "B1", "B2", "B3" ] }
    , { branchOffset = 5, firstNodeNum = 2, nodes = [ "C2", "C3", "C4" ] }
    ]



-- Trickier tree
--
-- A1 A2 A3 A4 A5 A6 A7 A8
--  \ G2  |     \ D6     \ B9
--  |     \ E4 E5 E6 E7  \ C9
--  \ H2 H3     \ F6
--     \ I3 I4


trickierTree : T.Tree String
trickierTree =
    t "root"
        [ t "A1"
            [ t "A2"
                [ t "A3"
                    [ t "A4"
                        [ t "A5"
                            [ t "A6"
                                [ t "A7"
                                    [ t "A8"
                                        [ T.Locked
                                        , t "B9" []
                                        , t "C9" []
                                        ]
                                    ]
                                ]
                            , t "D6" []
                            ]
                        ]
                    , t "E4"
                        [ t "E5"
                            [ t "E6" [ t "E7" [] ]
                            , t "F6" []
                            ]
                        ]
                    ]
                ]
            , t "G2" []
            , t "H2"
                [ t "H3" []
                , t "I3" [ t "I4" [] ]
                ]
            ]
        ]


trickierTreeBranchesWithParents : List (BranchWithParents String)
trickierTreeBranchesWithParents =
    [ { nodes = [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8" ], parents = [] }
    , { nodes = [ "B9" ], parents = [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8" ] }
    , { nodes = [ "C9" ], parents = [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8" ] }
    , { nodes = [ "D6" ], parents = [ "root", "A1", "A2", "A3", "A4", "A5" ] }
    , { nodes = [ "E4", "E5", "E6", "E7" ], parents = [ "root", "A1", "A2", "A3" ] }
    , { nodes = [ "F6" ], parents = [ "root", "A1", "A2", "A3", "E4", "E5" ] }
    , { nodes = [ "G2" ], parents = [ "root", "A1" ] }
    , { nodes = [ "H2", "H3" ], parents = [ "root", "A1" ] }
    , { nodes = [ "I3", "I4" ], parents = [ "root", "A1", "H2" ] }
    ]


trickierTreeBranches : List (Branch String)
trickierTreeBranches =
    [ { parentBranch = 0, firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8" ] }
    , { parentBranch = 0, firstNodeNum = 9, nodes = [ "B9" ] }
    , { parentBranch = 0, firstNodeNum = 9, nodes = [ "C9" ] }
    , { parentBranch = 0, firstNodeNum = 6, nodes = [ "D6" ] }
    , { parentBranch = 0, firstNodeNum = 4, nodes = [ "E4", "E5", "E6", "E7" ] }
    , { parentBranch = 4, firstNodeNum = 6, nodes = [ "F6" ] }
    , { parentBranch = 0, firstNodeNum = 2, nodes = [ "G2" ] }
    , { parentBranch = 0, firstNodeNum = 2, nodes = [ "H2", "H3" ] }
    , { parentBranch = 7, firstNodeNum = 3, nodes = [ "I3", "I4" ] }
    ]


trickierTreePositioned : List (PositionedBranch String)
trickierTreePositioned =
    [ { branchOffset = 0, firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8" ] }
    , { branchOffset = 1, firstNodeNum = 9, nodes = [ "B9" ] }
    , { branchOffset = 2, firstNodeNum = 9, nodes = [ "C9" ] }
    , { branchOffset = 1, firstNodeNum = 6, nodes = [ "D6" ] }
    , { branchOffset = 2, firstNodeNum = 4, nodes = [ "E4", "E5", "E6", "E7" ] }
    , { branchOffset = 3, firstNodeNum = 6, nodes = [ "F6" ] }
    , { branchOffset = 1, firstNodeNum = 2, nodes = [ "G2" ] }
    , { branchOffset = 3, firstNodeNum = 2, nodes = [ "H2", "H3" ] }
    , { branchOffset = 4, firstNodeNum = 3, nodes = [ "I3", "I4" ] }
    ]



-- Tests...


testBuildBranchQueue : Test
testBuildBranchQueue =
    describe "Build branch queue"
        [ test "Can build branch queue from empty tree" <|
            \_ ->
                Expect.equal
                    [ { nodes = [ "root" ], parents = [] } ]
                    (buildBranchQueue empTree)
        , test "Can build branch queue from linear tree" <|
            \_ ->
                Expect.equal
                    [ { nodes = [ "root", "A1", "A2" ], parents = [] } ]
                    (buildBranchQueue linTree)
        , test "Can build branch queue from var tree" <|
            \_ ->
                Expect.equal
                    [ { nodes = [ "root", "A1" ], parents = [] }
                    , { nodes = [ "B1" ], parents = [ "root" ] }
                    , { nodes = [ "C1" ], parents = [ "root" ] }
                    ]
                    (buildBranchQueue varTree)
        , test "Can build branch queue from tricky tree" <|
            \_ ->
                Expect.equal trickyTreeBranchesWithParents
                    (buildBranchQueue <| T.makeTree TT.trickyTree)
        , test "Can build branch queue from trickier tree" <|
            \_ ->
                Expect.equal trickierTreeBranchesWithParents
                    (buildBranchQueue trickierTree)
        ]


testFindParentBranch : Test
testFindParentBranch =
    describe "Find parent branch"
        [ test "Can find parent branch for trickier tree" <|
            \_ ->
                Expect.equal 0
                    (findParentBranch trickierTreeBranchesWithParents
                        { nodes = [ "D6" ], parents = [ "root", "A1", "A2", "A3", "A4", "A5" ] }
                    )
        , test "Can find tricky parent branch for trickier tree" <|
            \_ ->
                Expect.equal 4
                    (findParentBranch trickierTreeBranchesWithParents
                        { nodes = [ "F6" ], parents = [ "root", "A1", "A2", "A3", "E4", "E5" ] }
                    )
        , test "Can find trickier parent branch for trickier tree" <|
            \_ ->
                Expect.equal 7
                    (findParentBranch trickierTreeBranchesWithParents
                        { nodes = [ "I3", "I4" ], parents = [ "root", "A1", "H2" ] }
                    )
        ]


testBuildBranchList : Test
testBuildBranchList =
    describe "Build branch list"
        [ test "Can build branch list for tricky tree" <|
            \_ ->
                Expect.equal trickyTreeBranches
                    (buildBranchList trickyTreeBranchesWithParents)
        , test "Can build branch list for trickier tree" <|
            \_ ->
                Expect.equal trickierTreeBranches
                    (buildBranchList trickierTreeBranchesWithParents)
        ]


testBuildPositionedBranches : Test
testBuildPositionedBranches =
    describe "Build positioned branches list"
        [ test "Can build positioned branch list for tricky tree" <|
            \_ ->
                Expect.equal trickyTreePositioned
                    (buildPositionedBranches trickyTreeBranches)
        , test "Can build positioned branch list for trickier tree" <|
            \_ ->
                Expect.equal trickierTreePositioned
                    (buildPositionedBranches trickierTreeBranches)
        ]
