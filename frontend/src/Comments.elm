module Comments exposing (..)

import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
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


type alias CreateComment =
    { comment : String
    }


createCommentEncoder : CreateComment -> E.Value
createCommentEncoder createComment =
    E.object [ ( "comment", E.string createComment.comment ) ]



-- Parse Clickable Things


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



-- Parse Comment


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
            if commentText == "" then
                acc

            else
                TextPart commentText :: acc


commentParts : G.Record -> String -> List CommentPart
commentParts record commentStr =
    commentStr
        |> clickableThings
        |> clickableParts record
        |> commentPartsHelper commentStr []
        |> List.reverse



-- Helper


findClickables : List CommentPart -> List ClickablePartData -> List ClickablePartData
findClickables parts acc =
    case parts of
        (ClickablePart cpd) :: tailParts ->
            findClickables tailParts (cpd :: acc)

        (TextPart _) :: tailParts ->
            findClickables tailParts acc

        [] ->
            acc



-- Publicly Useful Functions


getComment : G.Record -> CommentResponse -> Comment
getComment record commentResponse =
    { source = record.source
    , commentId = commentResponse.commentId
    , userId = commentResponse.userId
    , username = commentResponse.username
    , comment = commentParts record commentResponse.comment
    , created = commentResponse.created
    }


clickables : List CommentPart -> List ClickablePartData
clickables cps =
    findClickables cps [] |> List.reverse


prevClickable : Int -> Int
prevClickable current =
    if current <= 0 then
        current

    else
        current - 1


nextClickable : List CommentPart -> Int -> Int
nextClickable cps current =
    if current + 1 >= (List.length <| clickables cps) then
        current

    else
        current + 1


getClickableForOne : Int -> List CommentPart -> Maybe ClickablePartData
getClickableForOne clickablePos cps =
    List.Extra.getAt clickablePos <| clickables cps


getClickableForMany : Int -> Int -> List Comment -> Maybe ClickablePartData
getClickableForMany commentPos clickablePos comments =
    comments
        |> List.Extra.getAt commentPos
        |> Maybe.andThen (\c -> getClickableForOne clickablePos c.comment)



-- Adding things to comments


add : String -> String -> String
add toAdd comment =
    let
        maybeSpace =
            if String.endsWith " " comment || String.endsWith "\n" comment then
                ""

            else
                " "
    in
    comment ++ maybeSpace ++ toAdd


coordToChar : Int -> String
coordToChar n =
    if n >= 1 && n <= 26 then
        String.fromChar (Char.fromCode (n - 1 + Char.toCode 'a'))

    else if n >= 27 && n <= 52 then
        String.fromChar (Char.fromCode (n - 27 + Char.toCode 'A'))

    else
        ""


coordsToString : G.Coords -> String
coordsToString coords =
    coordToChar coords.x ++ String.fromInt coords.y


playToString : G.Play -> String
playToString play =
    G.maybeCoords play
        |> Maybe.map coordsToString
        |> Maybe.withDefault ""


moveToString : Int -> G.Move -> String
moveToString moveNum move =
    String.fromInt moveNum ++ "." ++ playToString move.play


variationToString : Int -> List G.Move -> String
variationToString from moves =
    let
        toCommentParts : Int -> List G.Move -> List String -> List String
        toCommentParts moveNum moveList acc =
            case moveList of
                [] ->
                    List.reverse acc

                h :: t ->
                    toCommentParts (moveNum + 1) t (moveToString moveNum h :: acc)
    in
    " |" ++ (toCommentParts from moves [] |> String.join " ")



-- View


viewTextPart : String -> H.Html msg
viewTextPart str =
    H.span []
        (String.split "\n" str
            |> List.map H.text
            |> List.intersperse (H.br [] [])
        )


viewClickablePart : msg -> Bool -> ClickablePartData -> H.Html msg
viewClickablePart jumpMsg highlight cpd =
    let
        class =
            if highlight then
                "game-highlight"

            else
                "game-clickable"
    in
    H.button [ HE.onClick jumpMsg, HA.class class ] [ H.text cpd.clickable ]


viewCommentPartsHelper : (Int -> msg) -> Maybe Int -> Int -> List (H.Html msg) -> List CommentPart -> List (H.Html msg)
viewCommentPartsHelper jumpMsg currentPos movePos acc parts =
    case parts of
        [] ->
            List.reverse acc

        (TextPart str) :: tailParts ->
            viewCommentPartsHelper jumpMsg
                currentPos
                movePos
                (viewTextPart str :: acc)
                tailParts

        (ClickablePart cpd) :: tailParts ->
            viewCommentPartsHelper jumpMsg
                currentPos
                (movePos + 1)
                (viewClickablePart (jumpMsg movePos) (Just movePos == currentPos) cpd :: acc)
                tailParts


viewCommentParts : (Int -> msg) -> Maybe Int -> List CommentPart -> List (H.Html msg)
viewCommentParts jumpMsg currentPos parts =
    viewCommentPartsHelper jumpMsg currentPos 0 [] parts


viewComment : (Int -> Int -> msg) -> Maybe Int -> Int -> Comment -> List (H.Html msg)
viewComment jumpMsg currentPos commentPos comment =
    let
        info =
            H.div [ HA.class "comment-info" ]
                [ H.span [ HA.class "created" ] [ H.text comment.created ]
                , H.span [ HA.class "username" ] [ H.text comment.username ]
                ]

        parts =
            H.div [ HA.class "comment-parts" ]
                (viewCommentParts (jumpMsg commentPos) currentPos comment.comment)
    in
    [ H.div [ HA.class "comment" ] [ info, parts ] ]


view : (Int -> Int -> msg) -> Maybe ( Int, Int ) -> List Comment -> List (H.Html msg)
view jumpMsg current comments =
    let
        maybeCommentMove : Int -> Maybe Int
        maybeCommentMove commentPos =
            current
                |> Maybe.andThen
                    (\( c, p ) ->
                        if c == commentPos then
                            Just p

                        else
                            Nothing
                    )
    in
    List.indexedMap (\p c -> viewComment jumpMsg (maybeCommentMove p) p c) comments
        |> List.concat
