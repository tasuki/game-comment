module Replay.TreeTests exposing (..)

import Expect
import Replay.Tree exposing (..)
import Test exposing (..)


testTree : Tree String
testTree =
    listToLockedTree [ "main1", "main2", "main3", "main4" ]


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
                        |> lookNext
                        |> maybeZipperValue
                    )
        , test "Move next next ends at second move" <|
            \_ ->
                Expect.equal
                    (Just "main2")
                    (zipper
                        |> lookNext
                        |> Maybe.andThen lookNext
                        |> maybeZipperValue
                    )
        , test "Move end ends at end" <|
            \_ ->
                Expect.equal
                    (Just "main4")
                    (zipper
                        |> lookEnd
                        |> zipperValue
                    )
        , test "Move end+start ends at start" <|
            \_ ->
                Expect.equal
                    Nothing
                    (zipper
                        |> lookEnd
                        |> lookStart
                        |> zipperValue
                    )
        , test "Move end+prev ends one before last" <|
            \_ ->
                Expect.equal
                    (Just "main3")
                    (zipper
                        |> lookEnd
                        |> lookPrev
                        |> maybeZipperValue
                    )
        ]


var1Zip : Zipper String
var1Zip =
    zipper
        |> lookEnd
        |> (addChild "var1" >> Tuple.second)


gettingValuesTest : Test
gettingValuesTest =
    describe "Getting the values"
        [ test "Can get current values in var" <|
            \_ ->
                Expect.equal
                    (Just [ "main1", "main2", "main3", "main4", "var1" ])
                    (var1Zip |> lookNext |> Maybe.map currentValues)
        , test "Can get all values in var" <|
            \_ ->
                Expect.equal
                    (Just [ "main1", "main2", "main3", "main4", "var1" ])
                    (var1Zip |> lookNext |> Maybe.map allValues)
        , test "Can get current values in middle" <|
            \_ ->
                Expect.equal
                    (Just [ "main1", "main2", "main3" ])
                    (var1Zip |> lookPrev |> Maybe.map currentValues)
        , test "Can get all values in middle" <|
            \_ ->
                Expect.equal
                    (Just [ "main1", "main2", "main3", "main4", "var1" ])
                    (var1Zip |> lookPrev |> Maybe.map allValues)
        ]


varZipperTest : Test
varZipperTest =
    describe "Zipper test with variations"
        [ test "Can add a child at the end of main and descend to it" <|
            \_ ->
                Expect.equal
                    (Just "var1")
                    (zipper
                        |> lookEnd
                        |> (addChild "var1" >> Tuple.second)
                        |> lookNext
                        |> maybeZipperValue
                    )
        , test "Can add a child at the end of main, then start to go back to beginning" <|
            \_ ->
                Expect.equal
                    Nothing
                    (zipper
                        |> lookEnd
                        |> addChild "var1"
                        |> Tuple.second
                        |> lookNext
                        |> Maybe.map lookStart
                        |> maybeZipperValue
                    )
        , test "Can add a child at the end of main, then start+end to get to it again" <|
            \_ ->
                Expect.equal
                    (Just "var1")
                    (zipper
                        |> lookEnd
                        |> (addChild "var1" >> Tuple.second)
                        |> lookNext
                        |> Maybe.map lookStart
                        |> Maybe.map lookEnd
                        |> maybeZipperValue
                    )
        , test "Goes to the most recently added child at the end of main" <|
            \_ ->
                Expect.equal
                    (Just "var3")
                    (zipper
                        |> lookEnd
                        |> (addChild "var1" >> Tuple.second)
                        |> (addChild "var2" >> Tuple.second)
                        |> (addChild "var3" >> Tuple.second)
                        |> lookNext
                        |> maybeZipperValue
                    )
        , test "Goes to selected child at the end of main" <|
            \_ ->
                Expect.equal
                    (Just "var2")
                    (zipper
                        |> lookEnd
                        |> (addChild "var1" >> Tuple.second)
                        |> (addChild "var2" >> Tuple.second)
                        |> (addChild "var3" >> Tuple.second)
                        |> lookNextByIndex 2
                        |> maybeZipperValue
                    )
        , test "Can add a child at the start of main, then start+end to get to it again" <|
            \_ ->
                Expect.equal
                    (Just "var1")
                    (zipper
                        |> (addChild "var1" >> Tuple.second)
                        |> lookNext
                        |> Maybe.map lookStart
                        |> Maybe.map lookEnd
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
                        |> lookNext
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
                        |> lookNextByIndex 2
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
