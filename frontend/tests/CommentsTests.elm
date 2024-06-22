module CommentsTests exposing (..)

import Comments exposing (..)
import Expect
import GameRecord as G
import List.Extra
import Test exposing (..)


sampleComment =
    """
|17.j9 was very bad. Especially after 18.h9 19.k11, j9 is basically an empty triangle.
White is not going to force the 20.j11 21.j10 exchange now, but we can imagine it being there.

It would be superior to play |17.k11 instead. Now white has a severe cutting point at h9 to worry about.
The solid connection is already worse for white than the game. Any of the tiger mouths gets a peep.
Probably white would play 18.g8, but 19.h9 is immediately interesting.
If 20.g9, then 21.g10 22.h8 23.g11 24.h9 and black is very happy.

White would like to fight with 20.f11, but the push through with 21.g11 22.g10 23.f10 24.g9 25.e1 is clearly good for black.

What can white do instead? Perhaps 18.f11? But again black just pushes through and white has too many fires to take care of.

|20.i7 is also an empty triangle for white, but I don't think there's a way around it?

It feels like at |9.h1 the situation was already good for black.
10.i1 was a desperate attempt to create complications.
But I don't know what went wrong in the few moves before.

Also maybe |28.e8 was an overplay, 28.j5 would've been the honest move.
|29.i4 was very sharp, especially in combination with 31.j5 which I overlooked.
At that point the game might've been good for white regardless.
"""


clickableThingsTest : Test
clickableThingsTest =
    describe "clickableThings"
        [ test "Can parse clickable things out of a comment" <|
            \_ ->
                Expect.equal
                    [ ( "|17.j9", MoveFromMain (NumberedMove 17 { x = 10, y = 9 }) )
                    , ( "18.h9", Move (NumberedMove 18 { x = 8, y = 9 }) )
                    , ( "19.k11", Move (NumberedMove 19 { x = 11, y = 11 }) )
                    , ( "j9", Coords { x = 10, y = 9 } )
                    , ( "20.j11", Move (NumberedMove 20 { x = 10, y = 11 }) )
                    , ( "21.j10", Move (NumberedMove 21 { x = 10, y = 10 }) )
                    , ( "|17.k11", MoveFromMain (NumberedMove 17 { x = 11, y = 11 }) )
                    , ( "h9", Coords { x = 8, y = 9 } )
                    , ( "18.g8", Move (NumberedMove 18 { x = 7, y = 8 }) )
                    , ( "19.h9", Move (NumberedMove 19 { x = 8, y = 9 }) )
                    , ( "20.g9", Move (NumberedMove 20 { x = 7, y = 9 }) )
                    , ( "21.g10", Move (NumberedMove 21 { x = 7, y = 10 }) )
                    , ( "22.h8", Move (NumberedMove 22 { x = 8, y = 8 }) )
                    , ( "23.g11", Move (NumberedMove 23 { x = 7, y = 11 }) )
                    , ( "24.h9", Move (NumberedMove 24 { x = 8, y = 9 }) )
                    , ( "20.f11", Move (NumberedMove 20 { x = 6, y = 11 }) )
                    , ( "21.g11", Move (NumberedMove 21 { x = 7, y = 11 }) )
                    , ( "22.g10", Move (NumberedMove 22 { x = 7, y = 10 }) )
                    , ( "23.f10", Move (NumberedMove 23 { x = 6, y = 10 }) )
                    , ( "24.g9", Move (NumberedMove 24 { x = 7, y = 9 }) )
                    , ( "25.e1", Move (NumberedMove 25 { x = 5, y = 1 }) )
                    , ( "18.f11", Move (NumberedMove 18 { x = 6, y = 11 }) )
                    , ( "|20.i7", MoveFromMain (NumberedMove 20 { x = 9, y = 7 }) )
                    , ( "|9.h1", MoveFromMain (NumberedMove 9 { x = 8, y = 1 }) )
                    , ( "10.i1", Move (NumberedMove 10 { x = 9, y = 1 }) )
                    , ( "|28.e8", MoveFromMain (NumberedMove 28 { x = 5, y = 8 }) )
                    , ( "28.j5", Move (NumberedMove 28 { x = 10, y = 5 }) )
                    , ( "|29.i4", MoveFromMain (NumberedMove 29 { x = 9, y = 4 }) )
                    , ( "31.j5", Move (NumberedMove 31 { x = 10, y = 5 }) )
                    ]
                    (clickableThings sampleComment)
        ]


createMove : G.Player -> Int -> Int -> G.Move
createMove player x y =
    G.Move player (G.Place { x = x, y = y })


gameRecord : G.Record
gameRecord =
    let
        g =
            G.empty G.Go 19
    in
    { g
        | moves =
            List.range 1 5
                |> List.map (\i -> createMove (G.onMove G.Go <| i - 1) i i)
    }


takeFromRecord : Int -> List G.Move
takeFromRecord n =
    List.take n gameRecord.moves


testClickableThings : String
testClickableThings =
    "Hi |4.a2, 5.b3, 6.c4 or 6.c5 or even 5.b4. |5.g5 then i9 6.g6 and take the rest."


clickableExpectParts : List ClickablePartData
clickableExpectParts =
    [ ClickablePartData "|4.a2"
        (takeFromRecord 3
            ++ [ createMove G.White 1 2
               ]
        )
        Nothing
    , ClickablePartData "5.b3"
        (takeFromRecord 3
            ++ [ createMove G.White 1 2
               , createMove G.Black 2 3
               ]
        )
        Nothing
    , ClickablePartData "6.c4"
        (takeFromRecord 3
            ++ [ createMove G.White 1 2
               , createMove G.Black 2 3
               , createMove G.White 3 4
               ]
        )
        Nothing
    , ClickablePartData "6.c5"
        (takeFromRecord 3
            ++ [ createMove G.White 1 2
               , createMove G.Black 2 3
               , createMove G.White 3 5
               ]
        )
        Nothing
    , ClickablePartData "5.b4"
        (takeFromRecord 3
            ++ [ createMove G.White 1 2
               , createMove G.Black 2 4
               ]
        )
        Nothing
    , ClickablePartData "|5.g5"
        (takeFromRecord 4
            ++ [ createMove G.Black 7 5
               ]
        )
        Nothing
    , ClickablePartData "i9"
        (takeFromRecord 4
            ++ [ createMove G.Black 7 5
               ]
        )
        (Just <| G.Coords 9 9)
    , ClickablePartData "6.g6"
        (takeFromRecord 4
            ++ [ createMove G.Black 7 5
               , createMove G.White 7 6
               ]
        )
        Nothing
    ]


clickablePartsTest : Test
clickablePartsTest =
    describe "clickableParts"
        [ test "Can parse clickable things out of a comment" <|
            \_ ->
                Expect.equal
                    clickableExpectParts
                    (clickableParts
                        gameRecord
                        (clickableThings testClickableThings)
                    )
        ]


getPart : Int -> CommentPart
getPart i =
    List.Extra.getAt i clickableExpectParts
        |> Maybe.withDefault (ClickablePartData "" [] Nothing)
        |> ClickablePart


getCommentContentTest : Test
getCommentContentTest =
    describe "getCommentContentTest"
        [ test "Return the parsed comment" <|
            \_ ->
                Expect.equal
                    [ TextPart "Hi "
                    , getPart 0
                    , TextPart ", "
                    , getPart 1
                    , TextPart ", "
                    , getPart 2
                    , TextPart " or "
                    , getPart 3
                    , TextPart " or even "
                    , getPart 4
                    , TextPart ". "
                    , getPart 5
                    , TextPart " then "
                    , getPart 6
                    , TextPart " "
                    , getPart 7
                    , TextPart " and take the rest."
                    ]
                    (commentParts gameRecord testClickableThings)
        ]
