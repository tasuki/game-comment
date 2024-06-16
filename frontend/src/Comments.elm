module Comments exposing (..)

import GameRecord as G
import Json.Decode as D
import Maybe.Extra
import Parser exposing (..)
import Regex
import Replay.GameTree as GT


type alias CommentResponse =
    { commentId : Int
    , userId : Int
    , username : String
    , comment : String
    , created : String
    }


type alias CreateComment =
    { comment : String
    }


commentDecoder : D.Decoder CommentResponse
commentDecoder =
    D.map5 CommentResponse
        (D.field "commentId" D.int)
        (D.field "userId" D.int)
        (D.field "username" D.string)
        (D.field "comment" D.string)
        (D.field "created" D.string)


commentsDecoder : D.Decoder (List CommentResponse)
commentsDecoder =
    D.list commentDecoder



-- Parsing comments


type NumberedMove
    = NumberedMove Int G.Coords


type ClickableThing
    = MoveFromMain NumberedMove
    | Move NumberedMove
    | Coords G.Coords


type alias ClickableThings =
    List ClickableThing


type CommentPart
    = Text String
    | Clickable GT.GameView


type ParsedComment
    = List CommentPart


regex : Regex.Regex
regex =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = False, multiline = True }
            """(^| )(\\|?)((\\d+)\\.)?([a-zA-Z])(\\d{1,2})"""


charToCoord : String -> Maybe Int
charToCoord str =
    let
        charCodeToCoord code =
            if code >= Char.toCode 'a' && code <= Char.toCode 'z' then
                Just (code - Char.toCode 'a' + 1)

            else if code >= Char.toCode 'A' && code <= Char.toCode 'Z' then
                Just (code - Char.toCode 'A' + 27)

            else
                Nothing
    in
    case String.uncons str of
        Just ( c, "" ) ->
            charCodeToCoord <| Char.toCode c

        _ ->
            Nothing


createCommentPart : Bool -> Maybe Int -> G.Coords -> Maybe ClickableThing
createCommentPart offMain moveNum coords =
    case ( offMain, moveNum ) of
        ( True, Just m ) ->
            Just <| MoveFromMain <| NumberedMove m coords

        ( False, Just m ) ->
            Just <| Move <| NumberedMove m coords

        ( False, Nothing ) ->
            Just <| Coords coords

        ( True, Nothing ) ->
            -- "|r3" does not make sense in our context
            Nothing


type alias MS =
    Maybe String


processUnsanitized : MS -> MS -> MS -> MS -> Maybe ClickableThing
processUnsanitized offMain moveNumStr x y =
    case ( Maybe.andThen charToCoord x, Maybe.andThen String.toInt y ) of
        ( Just xInt, Just yInt ) ->
            createCommentPart
                (Maybe.Extra.isJust offMain)
                (Maybe.andThen String.toInt moveNumStr)
                { x = xInt, y = yInt }

        _ ->
            Nothing


processSubmatches : List (Maybe String) -> Maybe ClickableThing
processSubmatches matches =
    case matches of
        [ _, offMain, _, moveNumStr, xStr, yStr ] ->
            processUnsanitized offMain moveNumStr xStr yStr

        _ ->
            Nothing


clickableThings : String -> ClickableThings
clickableThings comment =
    Regex.find regex comment
        |> List.map .submatches
        |> List.filterMap processSubmatches
