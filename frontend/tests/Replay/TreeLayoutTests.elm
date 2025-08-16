module Replay.TreeLayoutTests exposing (..)

import Dict exposing (Dict)
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


linMaybeTree : T.Tree (Maybe String)
linMaybeTree =
    t Nothing [ t (Just "A1") [ t (Just "A2") [] ] ]


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
    [ { parentBranch = 0, branchOffset = 0, firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3" ] }
    , { parentBranch = 0, branchOffset = 1, firstNodeNum = 4, nodes = [ "E4" ] }
    , { parentBranch = 0, branchOffset = 2, firstNodeNum = 4, nodes = [ "F4", "F5", "F6" ] }
    , { parentBranch = 0, branchOffset = 3, firstNodeNum = 4, nodes = [ "G4" ] }
    , { parentBranch = 0, branchOffset = 4, firstNodeNum = 1, nodes = [ "B1", "B2", "B3" ] }
    , { parentBranch = 4, branchOffset = 5, firstNodeNum = 2, nodes = [ "C2", "C3", "C4" ] }
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
    [ { parentBranch = 0, branchOffset = 0, firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8" ] }
    , { parentBranch = 0, branchOffset = 1, firstNodeNum = 9, nodes = [ "B9" ] }
    , { parentBranch = 0, branchOffset = 2, firstNodeNum = 9, nodes = [ "C9" ] }
    , { parentBranch = 0, branchOffset = 1, firstNodeNum = 6, nodes = [ "D6" ] }
    , { parentBranch = 0, branchOffset = 2, firstNodeNum = 4, nodes = [ "E4", "E5", "E6", "E7" ] }
    , { parentBranch = 4, branchOffset = 3, firstNodeNum = 6, nodes = [ "F6" ] }
    , { parentBranch = 0, branchOffset = 1, firstNodeNum = 2, nodes = [ "G2" ] }
    , { parentBranch = 0, branchOffset = 3, firstNodeNum = 2, nodes = [ "H2", "H3" ] }
    , { parentBranch = 7, branchOffset = 4, firstNodeNum = 3, nodes = [ "I3", "I4" ] }
    ]



-- Trickiest tree
--
-- A1 A2 A3 A4 A5 A6 A7 A8
--  \ G2  |     \ D6     \ B9
--  |     \ E4 E5 E6 E7  \ C9
--  |     |     \ F6
--  |     \ X4 X5
--  \ H2 H3
--     \ I3 I4


trickiestTree : T.Tree String
trickiestTree =
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
                    , t "X4" [ t "X5" [] ]
                    ]
                ]
            , t "G2" []
            , t "H2"
                [ t "H3" []
                , t "I3" [ t "I4" [] ]
                ]
            ]
        ]


trickiestTreePositioned : List (PositionedBranch String)
trickiestTreePositioned =
    [ { parentBranch = 0, branchOffset = 0, firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8" ] }
    , { parentBranch = 0, branchOffset = 1, firstNodeNum = 9, nodes = [ "B9" ] }
    , { parentBranch = 0, branchOffset = 2, firstNodeNum = 9, nodes = [ "C9" ] }
    , { parentBranch = 0, branchOffset = 1, firstNodeNum = 6, nodes = [ "D6" ] }
    , { parentBranch = 0, branchOffset = 2, firstNodeNum = 4, nodes = [ "E4", "E5", "E6", "E7" ] }
    , { parentBranch = 4, branchOffset = 3, firstNodeNum = 6, nodes = [ "F6" ] }
    , { parentBranch = 0, branchOffset = 4, firstNodeNum = 4, nodes = [ "X4", "X5" ] }
    , { parentBranch = 0, branchOffset = 1, firstNodeNum = 2, nodes = [ "G2" ] }
    , { parentBranch = 0, branchOffset = 5, firstNodeNum = 2, nodes = [ "H2", "H3" ] }
    , { parentBranch = 8, branchOffset = 6, firstNodeNum = 3, nodes = [ "I3", "I4" ] }
    ]



-- An open question...
--
-- A1
--  \ B2 B3 B4
--  \ D2  \ C4


anOpenQuestion : T.Tree String
anOpenQuestion =
    t "root"
        [ t "A1"
            [ T.Locked
            , t "B2"
                [ t "B3"
                    [ t "B4" []
                    , t "C4" []
                    ]
                ]
            , t "D2" []
            ]
        ]


anOpenQuestionPositioned : List (PositionedBranch String)
anOpenQuestionPositioned =
    [ { parentBranch = 0, branchOffset = 0, firstNodeNum = 0, nodes = [ "root", "A1" ] }
    , { parentBranch = 0, branchOffset = 1, firstNodeNum = 2, nodes = [ "B2", "B3", "B4" ] }
    , { parentBranch = 1, branchOffset = 2, firstNodeNum = 4, nodes = [ "C4" ] }
    , { parentBranch = 0, branchOffset = 2, firstNodeNum = 2, nodes = [ "D2" ] }
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


testTreeToPositionedBranches : Test
testTreeToPositionedBranches =
    describe "Tree to positioned branches"
        [ test "Can convert trickyTree to positioned branches" <|
            \_ ->
                Expect.equal trickyTreePositioned
                    (treeToBranches <| T.makeTree TT.trickyTree)
        , test "Can convert trickerTree to positioned branches" <|
            \_ ->
                Expect.equal trickierTreePositioned
                    (treeToBranches trickierTree)
        , test "Can convert trickestTree to positioned branches" <|
            \_ ->
                Expect.equal trickiestTreePositioned
                    (treeToBranches trickiestTree)
        , test "Can convert anOpenQuestion to positioned branches" <|
            \_ ->
                Expect.equal anOpenQuestionPositioned
                    (treeToBranches anOpenQuestion)
        , test "Can convert linTree to positioned branches" <|
            \_ ->
                Expect.equal
                    [ { parentBranch = 0, branchOffset = 0, firstNodeNum = 0, nodes = [ "root", "A1", "A2" ] } ]
                    (treeToBranches linTree)
        , test "Can convert linMaybeTree to positioned branches" <|
            \_ ->
                Expect.equal
                    [ { parentBranch = 0, branchOffset = 0, firstNodeNum = 0, nodes = [ Nothing, Just "A1", Just "A2" ] } ]
                    (treeToBranches linMaybeTree)
        ]


testBranchesToDict : Test
testBranchesToDict =
    describe "Branches to dictionary"
        [ test "Can convert trickiestTree to dictionary" <|
            \_ ->
                Expect.all
                    [ \d -> Expect.equal (Just ( ( 0, -1 ), "root" )) (Dict.get ( 0, 0 ) d)
                    , \d -> Expect.equal (Just ( ( 0, 0 ), "A1" )) (Dict.get ( 0, 1 ) d)
                    , \d -> Expect.equal (Just ( ( 0, 1 ), "A2" )) (Dict.get ( 0, 2 ) d)
                    , \d -> Expect.equal (Just ( ( 0, 7 ), "A8" )) (Dict.get ( 0, 8 ) d)
                    , \d -> Expect.equal Nothing (Dict.get ( 0, 9 ) d)
                    , \d -> Expect.equal Nothing (Dict.get ( 1, 0 ) d)
                    , \d -> Expect.equal (Just ( ( 0, 8 ), "B9" )) (Dict.get ( 1, 9 ) d)
                    , \d -> Expect.equal (Just ( ( 0, 8 ), "C9" )) (Dict.get ( 2, 9 ) d)
                    , \d -> Expect.equal (Just ( ( 0, 3 ), "E4" )) (Dict.get ( 2, 4 ) d)
                    , \d -> Expect.equal (Just ( ( 2, 5 ), "F6" )) (Dict.get ( 3, 6 ) d)
                    , \d -> Expect.equal (Just ( ( 0, 3 ), "X4" )) (Dict.get ( 4, 4 ) d)
                    , \d -> Expect.equal (Just ( ( 4, 4 ), "X5" )) (Dict.get ( 4, 5 ) d)
                    , \d -> Expect.equal (Just ( ( 0, 1 ), "H2" )) (Dict.get ( 5, 2 ) d)
                    , \d -> Expect.equal (Just ( ( 5, 2 ), "I3" )) (Dict.get ( 6, 3 ) d)
                    ]
                    (branchesToDict trickiestTreePositioned)
        ]


testFindPosition : Test
testFindPosition =
    describe "Test findPosition"
        [ test "Can find position for first branch element" <|
            \_ ->
                Expect.equal ( 0, 2 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "A2" ])
        , test "Can find position for another first branch element" <|
            \_ ->
                Expect.equal ( 0, 5 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "A2", "A3", "A4", "A5" ])
        , test "Can find position for B branch element" <|
            \_ ->
                Expect.equal ( 1, 9 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "B9" ])
        , test "Can find position for C branch element" <|
            \_ ->
                Expect.equal ( 2, 9 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "C9" ])
        , test "Can find position for D branch element" <|
            \_ ->
                Expect.equal ( 1, 6 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "A2", "A3", "A4", "A5", "D6" ])
        , test "Can find position for F branch element" <|
            \_ ->
                Expect.equal ( 3, 6 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "A2", "A3", "E4", "E5", "F6" ])
        , test "Can find position for G branch element" <|
            \_ ->
                Expect.equal ( 1, 2 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "G2" ])
        , test "Can find position for H branch element" <|
            \_ ->
                Expect.equal ( 5, 2 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "H2" ])
        , test "Can find position for another H branch element" <|
            \_ ->
                Expect.equal ( 5, 3 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "H2", "H3" ])
        , test "Can find position for I branch element" <|
            \_ ->
                Expect.equal ( 6, 3 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "H2", "I3" ])
        , test "Can't find position for nonexistent element" <|
            \_ ->
                Expect.equal ( -1, -1 )
                    (findPosition trickiestTreePositioned [ "root", "A1", "A2", "I3" ])
        ]


trickiestTreeZipper : T.Zipper String
trickiestTreeZipper =
    T.makeZipper trickiestTree
        |> T.descend
        |> Maybe.andThen T.descend
        |> Maybe.andThen T.descend
        |> Maybe.andThen T.descend
        |> Maybe.withDefault (T.makeZipper T.Locked)


testGetTreeLayout : Test
testGetTreeLayout =
    describe "Get tree layout"
        [ test "Can get tree layout" <|
            \_ ->
                Expect.equal
                    [ [ Just ( ( 0, 1 ), "A2" ), Just ( ( 0, 2 ), "A3" ), Just ( ( 0, 3 ), "A4" ), Just ( ( 0, 4 ), "A5" ), Just ( ( 0, 5 ), "A6" ) ]
                    , [ Just ( ( 0, 1 ), "G2" ), Nothing, Nothing, Nothing, Just ( ( 0, 5 ), "D6" ) ]
                    , [ Nothing, Nothing, Just ( ( 0, 3 ), "E4" ), Just ( ( 2, 4 ), "E5" ), Just ( ( 2, 5 ), "E6" ) ]
                    ]
                    (getTreeLayout 5 3 trickiestTreeZipper |> List.map (List.map (Maybe.map (\li -> ( li.parent, li.node )))))
        , test "Can get very simple tree layout" <|
            \_ ->
                Expect.equal
                    [ [ Just { focus = True, node = "root", parent = ( 0, -1 ), path = [ "root" ] }
                      , Just { focus = False, node = "A1", parent = ( 0, 0 ), path = [ "root", "A1" ] }
                      , Just { focus = False, node = "A2", parent = ( 0, 1 ), path = [ "root", "A1", "A2" ] }
                      ]
                    , [ Nothing, Nothing, Nothing ]
                    , [ Nothing, Nothing, Nothing ]
                    ]
                    (getTreeLayout 3 3 (T.makeZipper linTree))
        , test "Can get maybe tree layout" <|
            \_ ->
                Expect.equal
                    [ [ Just { focus = True, node = Nothing, parent = ( 0, -1 ), path = [ Nothing ] }
                      , Just { focus = False, node = Just "A1", parent = ( 0, 0 ), path = [ Nothing, Just "A1" ] }
                      , Just { focus = False, node = Just "A2", parent = ( 0, 1 ), path = [ Nothing, Just "A1", Just "A2" ] }
                      ]
                    , [ Nothing, Nothing, Nothing ]
                    , [ Nothing, Nothing, Nothing ]
                    ]
                    (getTreeLayout 3 3 (T.makeZipper linMaybeTree))
        ]


testGetPath : Test
testGetPath =
    describe "Get path"
        [ test "Can get path" <|
            \_ ->
                Expect.equal
                    [ "root", "A1", "H2", "H3" ]
                    (getPath 5 3 trickiestTreePositioned)
        , test "Can get shorter path" <|
            \_ ->
                Expect.equal
                    [ "root", "A1", "H2" ]
                    (getPath 5 2 trickiestTreePositioned)
        , test "Can get simple path" <|
            \_ ->
                Expect.equal
                    [ "root", "A1", "A2" ]
                    (getPath 0 2 trickiestTreePositioned)
        , test "Can get complex path" <|
            \_ ->
                Expect.equal
                    [ "root", "A1", "H2", "I3", "I4" ]
                    (getPath 6 4 trickiestTreePositioned)
        , test "Can't get inexistent" <|
            \_ ->
                Expect.equal
                    []
                    (getPath 6 5 trickiestTreePositioned)
        , test "Can't get another inexistent" <|
            \_ ->
                Expect.equal
                    []
                    (getPath 6 2 trickiestTreePositioned)
        , test "Can get 0 1 path" <|
            \_ ->
                Expect.equal
                    [ "root", "A1" ]
                    (getPath 0 1 (treeToBranches linTree))
        , test "Can get 0 0 path" <|
            \_ ->
                Expect.equal
                    [ "root" ]
                    (getPath 0 0 (treeToBranches linTree))
        ]
