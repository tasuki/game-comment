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


testBuildBranchQueue : Test
testBuildBranchQueue =
    describe "Build branch queue"
        [ test "Can build branch queue from linear tree" <|
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
                Expect.equal
                    [ { nodes = [ "root", "A1", "A2", "A3" ], parents = [] }
                    , { nodes = [ "E4" ], parents = [ "root", "A1", "A2", "A3" ] }
                    , { nodes = [ "F4", "F5", "F6" ], parents = [ "root", "A1", "A2", "A3" ] }
                    , { nodes = [ "G4" ], parents = [ "root", "A1", "A2", "A3" ] }
                    , { nodes = [ "B1", "B2", "B3" ], parents = [ "root" ] }
                    , { nodes = [ "C2", "C3", "C4" ], parents = [ "root", "B1" ] }
                    ]
                    (buildBranchQueue <| T.makeTree TT.trickyTree)
        , test "Can build branch queue from trickier tree" <|
            \_ ->
                Expect.equal
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
                    (buildBranchQueue trickierTree)
        ]
