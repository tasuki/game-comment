module Comments exposing (..)

import Json.Decode as D


type alias Comment =
    { commentId : Int
    , userId : Int
    , username : String
    , comment : String
    , created : String
    }


commentDecoder : D.Decoder Comment
commentDecoder =
    D.map5 Comment
        (D.field "commentId" D.int)
        (D.field "userId" D.int)
        (D.field "username" D.string)
        (D.field "comment" D.string)
        (D.field "created" D.string)


commentsDecoder : D.Decoder (List Comment)
commentsDecoder =
    D.list commentDecoder
