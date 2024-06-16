module CommentsTests exposing (..)

import Comments exposing (..)
import Expect
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
                    [ MoveFromMain (NumberedMove 17 { x = 10, y = 9 })
                    , Move (NumberedMove 18 { x = 8, y = 9 })
                    , Move (NumberedMove 19 { x = 11, y = 11 })
                    , Coords { x = 10, y = 9 }
                    , Move (NumberedMove 20 { x = 10, y = 11 })
                    , Move (NumberedMove 21 { x = 10, y = 10 })
                    , MoveFromMain (NumberedMove 17 { x = 11, y = 11 })
                    , Coords { x = 8, y = 9 }
                    , Move (NumberedMove 18 { x = 7, y = 8 })
                    , Move (NumberedMove 19 { x = 8, y = 9 })
                    , Move (NumberedMove 20 { x = 7, y = 9 })
                    , Move (NumberedMove 21 { x = 7, y = 10 })
                    , Move (NumberedMove 22 { x = 8, y = 8 })
                    , Move (NumberedMove 23 { x = 7, y = 11 })
                    , Move (NumberedMove 24 { x = 8, y = 9 })
                    , Move (NumberedMove 20 { x = 6, y = 11 })
                    , Move (NumberedMove 21 { x = 7, y = 11 })
                    , Move (NumberedMove 22 { x = 7, y = 10 })
                    , Move (NumberedMove 23 { x = 6, y = 10 })
                    , Move (NumberedMove 24 { x = 7, y = 9 })
                    , Move (NumberedMove 25 { x = 5, y = 1 })
                    , Move (NumberedMove 18 { x = 6, y = 11 })
                    , MoveFromMain (NumberedMove 20 { x = 9, y = 7 })
                    , MoveFromMain (NumberedMove 9 { x = 8, y = 1 })
                    , Move (NumberedMove 10 { x = 9, y = 1 })
                    , MoveFromMain (NumberedMove 28 { x = 5, y = 8 })
                    , Move (NumberedMove 28 { x = 10, y = 5 })
                    , MoveFromMain (NumberedMove 29 { x = 9, y = 4 })
                    , Move (NumberedMove 31 { x = 10, y = 5 })
                    ]
                    (clickableThings sampleComment)
        ]
