module Comments exposing (..)

import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import List.Extra
import Maybe.Extra
import Regex



-- Fetching/creating comments


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



-- Clickable things


type NumberedMove
    = NumberedMove Int G.Coords


type ClickableThing
    = MoveFromMain NumberedMove
    | Move NumberedMove
    | Coords G.Coords


type alias ClickableThings =
    List ( String, ClickableThing )


moveRegex : Regex.Regex
moveRegex =
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


createClickableThing : Bool -> Maybe Int -> G.Coords -> Maybe ClickableThing
createClickableThing offMain moveNum coords =
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
            createClickableThing
                (Maybe.Extra.isJust offMain)
                (Maybe.andThen String.toInt moveNumStr)
                { x = xInt, y = yInt }

        _ ->
            Nothing


processSubmatches : List (Maybe String) -> Maybe ClickableThing
processSubmatches submatches =
    case submatches of
        [ _, offMain, _, moveNumStr, xStr, yStr ] ->
            processUnsanitized offMain moveNumStr xStr yStr

        _ ->
            Nothing


clickableThings : String -> ClickableThings
clickableThings comment =
    let
        matchToClickableThing : Regex.Match -> Maybe ( String, ClickableThing )
        matchToClickableThing m =
            processSubmatches m.submatches
                |> Maybe.map (\ct -> ( String.trim m.match, ct ))
    in
    Regex.find moveRegex comment
        |> List.filterMap matchToClickableThing



-- Viewable Comment


type alias ClickablePartData =
    { clickable : String
    , position : List G.Move
    , highlight : Maybe G.Coords
    }


type CommentPart
    = TextPart String
    | ClickablePart ClickablePartData


type alias Comment =
    { source : G.GameSource
    , commentId : Int
    , userId : Int
    , username : String
    , comment : List CommentPart
    , created : String
    }


thingToClickablePartData :
    List G.Move
    -> G.Record
    -> ( String, ClickableThing )
    -> ClickablePartData
thingToClickablePartData revMoves revRecord ( text, thing ) =
    let
        move moveNum coords =
            { player = G.onMove revRecord.game moveNum
            , play = G.Place coords
            }

        toDrop moveNum =
            List.length revRecord.moves - moveNum

        nextMove moveNum coords revMvs =
            if moveNum > List.length revMvs then
                move moveNum coords :: List.drop (toDrop moveNum) revRecord.moves

            else if moveNum < List.length revMvs then
                nextMove moveNum coords (List.tail revMvs |> Maybe.withDefault [])

            else
                move moveNum coords :: revMvs
    in
    case thing of
        MoveFromMain (NumberedMove moveNum coords) ->
            ClickablePartData text
                (move (moveNum - 1) coords :: List.drop (toDrop (moveNum - 1)) revRecord.moves)
                Nothing

        Move (NumberedMove moveNum coords) ->
            ClickablePartData text
                (nextMove (moveNum - 1) coords revMoves)
                Nothing

        Coords coords ->
            ClickablePartData text
                revMoves
                (Just coords)


clickablePartsHelper :
    ClickableThings
    -> G.Record
    -> List G.Move
    -> List ClickablePartData
    -> List ClickablePartData
clickablePartsHelper things revRecord revMoves acc =
    case things of
        t :: ts ->
            let
                newClickable : ClickablePartData
                newClickable =
                    thingToClickablePartData revMoves revRecord t
            in
            clickablePartsHelper ts
                revRecord
                newClickable.position
                (newClickable :: acc)

        [] ->
            List.reverse acc


clickableParts : G.Record -> ClickableThings -> List ClickablePartData
clickableParts record things =
    clickablePartsHelper things { record | moves = List.reverse record.moves } [] []
        |> List.map (\cpd -> { cpd | position = List.reverse cpd.position })


commentPartsHelper : String -> List CommentPart -> List ClickablePartData -> List CommentPart
commentPartsHelper commentText acc cpds =
    case cpds of
        cp :: cps ->
            case String.indexes cp.clickable commentText |> List.head of
                Just n ->
                    commentPartsHelper
                        (String.dropLeft (n + String.length cp.clickable) commentText)
                        (ClickablePart cp :: (TextPart <| String.left n commentText) :: acc)
                        cps

                Nothing ->
                    TextPart "Problem reassembling comment. Should never happen!" :: acc

        [] ->
            TextPart commentText :: acc


commentParts : G.Record -> String -> List CommentPart
commentParts record commentStr =
    commentStr
        |> clickableThings
        |> clickableParts record
        |> commentPartsHelper commentStr []
        |> List.reverse


getComment : G.Record -> CommentResponse -> Comment
getComment record commentResponse =
    { source = record.source
    , commentId = commentResponse.commentId
    , userId = commentResponse.userId
    , username = commentResponse.username
    , comment = commentParts record commentResponse.comment
    , created = commentResponse.created
    }


getClickable : Int -> Int -> List Comment -> Maybe ClickablePartData
getClickable commentPos clickablePos comments =
    let
        findClickable : Int -> List CommentPart -> Maybe ClickablePartData
        findClickable pos parts =
            case parts of
                (ClickablePart cpd) :: tailParts ->
                    if pos == 0 then
                        Just cpd

                    else
                        findClickable (pos - 1) tailParts

                (TextPart _) :: tailParts ->
                    findClickable pos tailParts

                [] ->
                    Nothing
    in
    comments
        |> List.Extra.getAt commentPos
        |> Maybe.andThen (\c -> findClickable clickablePos c.comment)



-- View


viewTextPart : String -> H.Html msg
viewTextPart str =
    H.span []
        (String.split "\n" str
            |> List.map H.text
            |> List.intersperse (H.br [] [])
        )


viewClickablePart : msg -> ClickablePartData -> H.Html msg
viewClickablePart jumpMsg cpd =
    H.button [ HE.onClick jumpMsg ] [ H.text cpd.clickable ]


viewCommentParts : (Int -> msg) -> Int -> List (H.Html msg) -> List CommentPart -> List (H.Html msg)
viewCommentParts jumpMsg movePos acc parts =
    case parts of
        [] ->
            List.reverse acc

        (TextPart str) :: tailParts ->
            viewCommentParts jumpMsg
                movePos
                (viewTextPart str :: acc)
                tailParts

        (ClickablePart cpd) :: tailParts ->
            viewCommentParts jumpMsg
                (movePos + 1)
                (viewClickablePart (jumpMsg movePos) cpd :: acc)
                tailParts


viewComment : (Int -> Int -> msg) -> Int -> Comment -> List (H.Html msg)
viewComment jumpMsg commentPos comment =
    viewCommentParts (jumpMsg commentPos) 0 [] comment.comment


view : (Int -> Int -> msg) -> List Comment -> List (H.Html msg)
view jumpMsg comments =
    List.indexedMap (viewComment jumpMsg) comments
        |> List.concat
