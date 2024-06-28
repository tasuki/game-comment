module Replay.TreeTests exposing (..)

import Expect
import Replay.Tree exposing (..)
import Test exposing (..)


testTree : Tree String
testTree =
    createLockedTree [ "main1", "main2", "main3", "main4" ]


zipper : Zipper String
zipper =
    makeZipper testTree


zipperValue : Zipper a -> Maybe a
zipperValue =
    .focus >> getValue


maybeZipperValue : Maybe (Zipper a) -> Maybe a
maybeZipperValue =
    Maybe.andThen zipperValue


convertToZipperTest : Test
convertToZipperTest =
    describe "Convert to zipper"
        [ test "Can convert to zipper and back" <|
            \_ -> Expect.equal testTree (makeZipper testTree |> makeTree)
        ]


basicZipperTest : Test
basicZipperTest =
    describe "Basic zipper"
        [ test "Points to root by default" <|
            \_ ->
                Expect.equal
                    Nothing
                    (zipper |> zipperValue)
        , test "Move next ends at first move" <|
            \_ ->
                Expect.equal
                    (Just "main1")
                    (zipper
                        |> descend
                        |> maybeZipperValue
                    )
        , test "Move next next ends at second move" <|
            \_ ->
                Expect.equal
                    (Just "main2")
                    (zipper
                        |> descend
                        |> Maybe.andThen descend
                        |> maybeZipperValue
                    )
        , test "Move end ends at end" <|
            \_ ->
                Expect.equal
                    (Just "main4")
                    (zipper
                        |> descendEnd
                        |> zipperValue
                    )
        , test "Move end+start ends at start" <|
            \_ ->
                Expect.equal
                    Nothing
                    (zipper
                        |> descendEnd
                        |> ascendStart
                        |> zipperValue
                    )
        , test "Move end+prev ends one before last" <|
            \_ ->
                Expect.equal
                    (Just "main3")
                    (zipper
                        |> descendEnd
                        |> ascend
                        |> maybeZipperValue
                    )
        ]


var1Zip : Zipper String
var1Zip =
    zipper
        |> descendEnd
        |> (addChild "var1" >> Tuple.second)


gettingValuesTest : Test
gettingValuesTest =
    describe "Getting the values"
        [ test "Can get current values in var" <|
            \_ ->
                Expect.equal
                    (Just [ "main1", "main2", "main3", "main4", "var1" ])
                    (var1Zip |> descend |> Maybe.map currentValues)
        , test "Can get all values in var" <|
            \_ ->
                Expect.equal
                    (Just [ "main1", "main2", "main3", "main4", "var1" ])
                    (var1Zip |> descend |> Maybe.map allValues)
        , test "Can get current values in middle" <|
            \_ ->
                Expect.equal
                    (Just [ "main1", "main2", "main3" ])
                    (var1Zip |> ascend |> Maybe.map currentValues)
        , test "Can get all values in middle" <|
            \_ ->
                Expect.equal
                    (Just [ "main1", "main2", "main3", "main4", "var1" ])
                    (var1Zip |> ascend |> Maybe.map allValues)
        ]


varZipperTest : Test
varZipperTest =
    describe "Zipper test with variations"
        [ test "Can add a child at the end of main and descend to it" <|
            \_ ->
                Expect.equal
                    (Just "var1")
                    (zipper
                        |> descendEnd
                        |> (addChild "var1" >> Tuple.second)
                        |> descend
                        |> maybeZipperValue
                    )
        , test "Can add a child at the end of main, then start to go back to beginning" <|
            \_ ->
                Expect.equal
                    Nothing
                    (zipper
                        |> descendEnd
                        |> addChild "var1"
                        |> Tuple.second
                        |> descend
                        |> Maybe.map ascendStart
                        |> maybeZipperValue
                    )
        , test "Can add a child at the end of main, then start+end to get to it again" <|
            \_ ->
                Expect.equal
                    (Just "var1")
                    (zipper
                        |> descendEnd
                        |> (addChild "var1" >> Tuple.second)
                        |> descend
                        |> Maybe.map ascendStart
                        |> Maybe.map descendEnd
                        |> maybeZipperValue
                    )
        , test "Goes to the most recently added child at the end of main" <|
            \_ ->
                Expect.equal
                    (Just "var3")
                    (zipper
                        |> descendEnd
                        |> (addChild "var1" >> Tuple.second)
                        |> (addChild "var2" >> Tuple.second)
                        |> (addChild "var3" >> Tuple.second)
                        |> descend
                        |> maybeZipperValue
                    )
        , test "Goes to selected child at the end of main" <|
            \_ ->
                Expect.equal
                    (Just "var2")
                    (zipper
                        |> descendEnd
                        |> (addChild "var1" >> Tuple.second)
                        |> (addChild "var2" >> Tuple.second)
                        |> (addChild "var3" >> Tuple.second)
                        |> descendToIndex 2
                        |> maybeZipperValue
                    )
        , test "Can add a child at the start of main, then start+end to get to it again" <|
            \_ ->
                Expect.equal
                    (Just "var1")
                    (zipper
                        |> (addChild "var1" >> Tuple.second)
                        |> descend
                        |> Maybe.map ascendStart
                        |> Maybe.map descendEnd
                        |> maybeZipperValue
                    )
        , test "Goes to the most recently added child at the start of main" <|
            \_ ->
                Expect.equal
                    (Just "var3")
                    (zipper
                        |> (addChild "var1" >> Tuple.second)
                        |> (addChild "var2" >> Tuple.second)
                        |> (addChild "var3" >> Tuple.second)
                        |> descend
                        |> maybeZipperValue
                    )
        , test "Goes to selected child at the start of main" <|
            \_ ->
                Expect.equal
                    (Just "var2")
                    (zipper
                        |> (addChild "var1" >> Tuple.second)
                        |> (addChild "var2" >> Tuple.second)
                        |> (addChild "var3" >> Tuple.second)
                        |> descendToIndex 2
                        |> maybeZipperValue
                    )
        ]



-- Brace!


t : a -> Forest a -> Tree a
t v c =
    Tree { value = Just v, defaultChild = 0, children = c }


trickyTree : Zipper String
trickyTree =
    t "root"
        [ t "A1"
            [ t "A2"
                [ t "A3"
                    [ Locked
                    , t "E4" []
                    , t "F4"
                        [ t "F5"
                            [ t "F6" []
                            ]
                        ]
                    , t "G4" []
                    ]
                ]
            ]
        , t "B1"
            [ t "B2"
                [ t "B3" []
                ]
            , t "C2"
                [ t "C3"
                    [ t "C4" []
                    ]
                ]
            ]
        ]
        |> makeZipper


replaceFirstTest : Test
replaceFirstTest =
    describe "replaceFirstVar"
        [ test "Can replace tricky tree with longer main var" <|
            let
                record : List (Maybe String)
                record =
                    List.map Just [ "root", "A1", "A2", "A3", "A4", "A5" ]
            in
            Expect.all
                [ \_ ->
                    Expect.equal
                        (t "root"
                            [ t "A1"
                                [ t "A2"
                                    [ t "A3"
                                        [ t "A4"
                                            [ t "A5"
                                                [ Locked
                                                ]
                                            ]
                                        , t "E4" []
                                        , t "F4"
                                            [ t "F5"
                                                [ t "F6" []
                                                ]
                                            ]
                                        , t "G4" []
                                        ]
                                    ]
                                ]
                            , t "B1"
                                [ t "B2"
                                    [ t "B3" []
                                    ]
                                , t "C2"
                                    [ t "C3"
                                        [ t "C4" []
                                        ]
                                    ]
                                ]
                            ]
                        )
                        (replaceFirstVar record (makeTree trickyTree))
                ]
        ]


findOne : String -> Zipper String -> Maybe (Zipper String)
findOne str zip =
    case findAll ((==) str) zip of
        [ z ] ->
            Just z

        _ ->
            Nothing


switchVariationCases : List ( String, String, String )
switchVariationCases =
    -- So far we only switch when the variations
    -- share the immediate direct ancestor.
    [ ( "root", "next", "root" )
    , ( "root", "prev", "root" )
    , ( "A1", "next", "B1" )
    , ( "B1", "next", "B1" )
    , ( "A1", "prev", "A1" )
    , ( "B1", "prev", "A1" )
    , ( "A2", "next", "A2" ) -- only local
    , ( "B2", "next", "C2" )
    , ( "C2", "next", "C2" )
    , ( "A2", "prev", "A2" )
    , ( "B2", "prev", "B2" ) -- only local
    , ( "C2", "prev", "B2" )
    ]


canSwitchVariation : ( String, String, String ) -> Test
canSwitchVariation ( from, dir, to ) =
    let
        fun : Zipper a -> Zipper a
        fun =
            case dir of
                "next" ->
                    nextVariation

                "prev" ->
                    prevVariation

                _ ->
                    identity
    in
    test ("Switch " ++ dir ++ " from " ++ from ++ " to " ++ to) <|
        \_ ->
            Expect.equal
                (Just to)
                (findOne from trickyTree
                    |> Maybe.map fun
                    |> maybeZipperValue
                )


switchVariationsTest : Test
switchVariationsTest =
    describe "Switch variations" <|
        List.map canSwitchVariation <|
            switchVariationCases
