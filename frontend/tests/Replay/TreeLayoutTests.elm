module Replay.TreeLayoutTests exposing (..)

import Expect
import Replay.Tree as T
import Replay.TreeLayout exposing (..)
import Replay.TreeTests as TT exposing (t)
import Test exposing (..)



-- trickyTree:
--
-- A1 A2 A3
--        \ E4
--        \ F4 F5 F6
--        \ G4
-- B1 B2 B3
--  \ C2 C3 C4


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



-- A1 A2 A3 A4 A5 A6 A7 A8
--  \ F2  |     \ D6     \ B9
--  |     \ E4 E5 E6 E7  \ C9
--  \ G2 G3
--     \ H3 H4


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
                    , t "E4" [ t "E5" [ t "E6" [ t "E7" [] ] ] ]
                    ]
                ]
            , t "F2" []
            , t "G2"
                [ t "G3" []
                , t "H3" [ t "H4" [] ]
                ]
            ]
        ]


testBuildBranchQueue : Test
testBuildBranchQueue =
    describe "Build branch queue"
        [ test "Can build branch queue from linear tree" <|
            \_ ->
                Expect.equal
                    [ { firstNodeNum = 0, nodes = [ "root", "A1", "A2" ], parentBranch = 0 } ]
                    (buildBranchQueue linTree)
        , test "Can build branch queue from var tree" <|
            \_ ->
                Expect.equal
                    [ { firstNodeNum = 0, nodes = [ "root", "A1" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "B1" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "C1" ], parentBranch = 0 }
                    ]
                    (buildBranchQueue varTree)
        , test "Can build branch queue from tricky tree" <|
            \_ ->
                Expect.equal
                    [ { firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "E4" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "F4", "F5", "F6" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "G4" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "B1", "B2", "B3" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "C2", "C3", "C4" ], parentBranch = 0 }
                    ]
                    (buildBranchQueue <| T.makeTree TT.trickyTree)
        , test "Can build branch queue from trickier tree" <|
            \_ ->
                Expect.equal
                    [ { firstNodeNum = 0, nodes = [ "root", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "B9" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "C9" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "D6" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "E4", "E5", "E6", "E7" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "F2" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "G2", "G3" ], parentBranch = 0 }
                    , { firstNodeNum = 0, nodes = [ "H3", "H4" ], parentBranch = 0 }
                    ]
                    (buildBranchQueue trickierTree)
        ]
