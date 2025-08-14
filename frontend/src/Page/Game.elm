module Page.Game exposing (..)

import ApiClient as AC
import Browser.Events
import Comments as C
import Game.Go
import Game.Hex
import Game.ToroidGo
import Game.TwixT
import GameHelpers as GH
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import List.Extra
import Maybe.Extra
import Page exposing (Page)
import Page.Help exposing (Model)
import Replay as R
import Replay.TreeLayout
import Session exposing (Session)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task



-- Viewing


type View
    = ViewReplay
    | ViewWipComment Int
    | ViewComment Int Int


hasWipComment : Model -> Bool
hasWipComment model =
    model.wipComment /= ""


getCommentParts : Model -> List C.CommentPart
getCommentParts model =
    case model.replay of
        Just replay ->
            C.commentParts replay.record model.wipComment

        _ ->
            []


viewPrevious : Model -> View
viewPrevious model =
    case model.view of
        ViewComment comment _ ->
            if comment > 0 then
                ViewComment (comment - 1) 0

            else if hasWipComment model then
                ViewWipComment 0

            else
                ViewReplay

        _ ->
            ViewReplay


viewNext : Model -> View
viewNext model =
    let
        commentsLength =
            List.length model.comments

        wipNext =
            if commentsLength > 0 then
                ViewComment 0 0

            else
                model.view
    in
    case model.view of
        ViewReplay ->
            if hasWipComment model then
                ViewWipComment 0

            else
                wipNext

        ViewWipComment _ ->
            wipNext

        ViewComment comment _ ->
            if comment < commentsLength - 1 then
                ViewComment (comment + 1) 0

            else
                model.view


currentComment : View -> Maybe ( Int, Int )
currentComment v =
    case v of
        ViewComment c m ->
            Just ( c, m )

        _ ->
            Nothing



-- MODEL


type alias Model =
    { session : Session
    , view : View
    , source : Maybe G.GameSource
    , replay : Maybe R.Replay
    , wipCommentBeingEdited : Bool
    , wipComment : String
    , comments : List C.Comment
    , message : String
    }


sidebarMsg : String
sidebarMsg =
    "Use the left/right keys to explore the game."


initEmpty : G.Game -> Int -> Session -> ( Model, Cmd Msg )
initEmpty game size session =
    ( { session = session
      , view = ViewReplay
      , source = Nothing
      , replay = Just <| R.emptyReplay <| G.empty game size
      , wipCommentBeingEdited = False
      , wipComment = ""
      , comments = []
      , message = sidebarMsg
      }
    , Cmd.none
    )


initGame : G.GameSource -> Session -> ( Model, Cmd Msg )
initGame gameSource session =
    ( { session = session
      , view = ViewReplay
      , source = Just gameSource
      , replay = Nothing
      , wipCommentBeingEdited = False
      , wipComment = ""
      , comments = []
      , message = sidebarMsg
      }
    , AC.getSgf Fetched gameSource
    )


initPrevious : G.GameSource -> Maybe R.Replay -> String -> List C.Comment -> Session -> ( Model, Cmd Msg )
initPrevious gameSource maybeReplay wipComment comments session =
    ( { session = session
      , view = ViewReplay -- TODO preserve previous view?
      , source = Just gameSource
      , replay = maybeReplay
      , wipCommentBeingEdited = False
      , wipComment = wipComment
      , comments = comments
      , message = ""
      }
    , Task.succeed Reload |> Task.perform identity
    )



-- UPDATE


type Msg
    = Noop
    | Reload
    | Fetched AC.SgfResult
    | FetchedComments G.GameSource AC.CommentsResult
    | PrevView
    | NextView
    | Play G.Coords
    | Forward
    | Backward
    | Start
    | End
    | Jump View
    | PrevVariation
    | NextVariation
    | CutVariation
    | CommentFocus
    | CommentBlur
    | CommentEdit String
    | CommentAddVariation
    | CommentAddCoords G.Coords
    | CreateComment
    | CommentCreated AC.CommentCreatedResult


onlyInReplay : Model -> Model -> ( Model, Cmd msg )
onlyInReplay model new =
    case model.view of
        ViewReplay ->
            ( new, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Reload ->
            case model.source of
                Just gameSource ->
                    ( model
                    , AC.getSgf Fetched gameSource
                    )

                _ ->
                    ( model, Cmd.none )

        Fetched result ->
            case result of
                Ok record ->
                    if Just record.source == model.source then
                        ( { model | replay = Just <| R.withRecord record model.replay }
                        , AC.getComments (FetchedComments record.source) record.source
                        )

                    else
                        ( model, Cmd.none )

                Err error ->
                    ( { model | message = "Could not load game: [ " ++ error ++ " ]" }
                    , Cmd.none
                    )

        FetchedComments source result ->
            case result of
                Ok comments ->
                    if Just source == model.source then
                        case model.replay of
                            Just replay ->
                                ( { model | comments = List.map (C.getComment replay.record) comments }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model | message = "No replay, so not showing comments..." }
                                , Cmd.none
                                )

                    else
                        ( { model | message = "Comments for a different game" }
                        , Cmd.none
                        )

                Err error ->
                    ( { model | message = "Could not load comments: [ " ++ error ++ " ]" }
                    , Cmd.none
                    )

        PrevView ->
            ( { model | view = viewPrevious model }, Cmd.none )

        NextView ->
            ( { model | view = viewNext model }, Cmd.none )

        Jump jumpView ->
            ( { model | view = jumpView }, Cmd.none )

        Backward ->
            case model.view of
                ViewComment cPos clickable ->
                    ( { model | view = ViewComment cPos (C.prevClickable clickable) }, Cmd.none )

                ViewWipComment clickable ->
                    ( { model | view = ViewWipComment (C.prevClickable clickable) }, Cmd.none )

                _ ->
                    ( { model | replay = Maybe.map R.prev model.replay }, Cmd.none )

        Forward ->
            case model.view of
                ViewComment cPos clickable ->
                    case List.Extra.getAt cPos model.comments of
                        Just comment ->
                            ( { model | view = ViewComment cPos (C.nextClickable comment.comment clickable) }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                ViewWipComment clickable ->
                    ( { model | view = ViewWipComment (C.nextClickable (getCommentParts model) clickable) }, Cmd.none )

                _ ->
                    ( { model | replay = Maybe.map R.next model.replay }, Cmd.none )

        Start ->
            onlyInReplay model { model | replay = Maybe.map R.start model.replay }

        End ->
            onlyInReplay model { model | replay = Maybe.map R.end model.replay }

        PrevVariation ->
            onlyInReplay model { model | replay = Maybe.map R.prevVariation model.replay }

        NextVariation ->
            onlyInReplay model { model | replay = Maybe.map R.nextVariation model.replay }

        CutVariation ->
            onlyInReplay model { model | replay = Maybe.map R.cutVariation model.replay }

        Play coords ->
            let
                isMoveLegal replay =
                    case replay.record.game of
                        G.ToroidGo ->
                            Game.ToroidGo.isLegal coords replay

                        G.Go ->
                            Game.Go.isLegal coords replay

                        _ ->
                            True
            in
            onlyInReplay model <|
                case Maybe.map isMoveLegal model.replay of
                    Just True ->
                        { model | replay = Maybe.map (R.playCoords coords) model.replay }

                    _ ->
                        model

        CommentFocus ->
            ( { model | wipCommentBeingEdited = True }, Cmd.none )

        CommentBlur ->
            ( { model | wipCommentBeingEdited = False }, Cmd.none )

        CommentEdit comment ->
            ( { model | wipComment = comment }
            , Cmd.none
            )

        CommentAddVariation ->
            let
                append =
                    Maybe.map R.currentVariation model.replay
                        |> Maybe.map (\( n, ms ) -> C.variationToString n ms)
                        |> Maybe.withDefault ""
            in
            ( { model | wipComment = C.add append model.wipComment }
            , Cmd.none
            )

        CommentAddCoords coords ->
            let
                moveNum =
                    case model.replay of
                        Just r ->
                            R.currentMoves r
                                |> R.findMoveNumber coords
                                |> Maybe.Extra.filter (\mn -> Just mn == R.findMoveNumber coords r.record.moves)
                                |> Maybe.map (\mn -> "|" ++ String.fromInt mn ++ ".")
                                |> Maybe.withDefault ""

                        _ ->
                            ""

                append =
                    moveNum ++ C.coordsToString coords
            in
            ( { model | wipComment = C.add append model.wipComment }
            , Cmd.none
            )

        CreateComment ->
            case model.source of
                Just gameSource ->
                    ( { model | wipComment = "" }
                    , AC.createComment
                        CommentCreated
                        model.session
                        gameSource
                        { comment = model.wipComment }
                    )

                Nothing ->
                    ( model, Cmd.none )

        CommentCreated result ->
            -- TODO maybe do something if something broke or didn't?
            update Reload model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.wipCommentBeingEdited then
        Sub.none

    else
        Sub.batch
            [ Browser.Events.onKeyDown (D.map keydown <| D.field "key" D.string) ]


keydown : String -> Msg
keydown keycode =
    case keycode of
        "ArrowLeft" ->
            Backward

        "ArrowRight" ->
            Forward

        "ArrowUp" ->
            PrevView

        "ArrowDown" ->
            NextView

        "h" ->
            Backward

        "l" ->
            Forward

        "k" ->
            PrevVariation

        "j" ->
            NextVariation

        "x" ->
            CutVariation

        "g" ->
            Start

        "G" ->
            End

        _ ->
            Noop



-- VIEW


squareIcon : List (Svg msg) -> H.Html msg
squareIcon svgElems =
    Svg.svg [ SA.width "40", SA.height "40" ] svgElems


start : H.Html msg
start =
    squareIcon
        [ Svg.polygon [ SA.points "14,13 14,27 12,27 12,13" ] []
        , Svg.polygon [ SA.points "28,12 28,28 15,20" ] []
        ]


end : H.Html msg
end =
    squareIcon
        [ Svg.polygon [ SA.points "26,13 26,27 28,27 28,13" ] []
        , Svg.polygon [ SA.points "12,12 12,28 25,20" ] []
        ]


backward : H.Html msg
backward =
    squareIcon
        [ Svg.polygon [ SA.points "25,12 25,28 12,20" ] [] ]


forward : H.Html msg
forward =
    squareIcon
        [ Svg.polygon [ SA.points "15,12 15,28 28,20" ] [] ]


boardView : Model -> H.Html Msg
boardView model =
    let
        specificView : R.Replay -> GH.GameView msg
        specificView replay =
            case replay.record.game of
                G.TwixT ->
                    Game.TwixT.view

                G.ToroidGo ->
                    Game.ToroidGo.view

                G.Go ->
                    Game.Go.view

                G.Hex ->
                    Game.Hex.view

        highlightLastMove : C.ClickablePartData -> Maybe G.Move
        highlightLastMove cpd =
            if Maybe.Extra.isJust cpd.highlight then
                Nothing

            else
                List.Extra.last cpd.position

        viewComment : R.Replay -> C.ClickablePartData -> Svg Msg
        viewComment replay cpd =
            specificView replay
                replay.record.size
                cpd.position
                cpd.highlight
                (R.currentColour replay)
                []
                (highlightLastMove cpd)
                (G.onMove replay.record.game <| List.length cpd.position)
                Play
                CommentAddCoords

        viewReplay : R.Replay -> Svg Msg
        viewReplay replay =
            specificView replay
                replay.record.size
                (R.currentMoves replay)
                Nothing
                (R.currentColour replay)
                (R.children replay)
                (R.lastPlayed replay)
                (R.onMove replay.record.game replay)
                Play
                CommentAddCoords
    in
    case model.replay of
        Just replay ->
            case model.view of
                ViewReplay ->
                    viewReplay replay

                ViewWipComment clickablePos ->
                    C.getClickableForOne clickablePos (getCommentParts model)
                        |> Maybe.map (viewComment replay)
                        |> Maybe.withDefault (viewReplay replay)

                ViewComment commentPos clickablePos ->
                    C.getClickableForMany commentPos clickablePos model.comments
                        |> Maybe.map (viewComment replay)
                        |> Maybe.withDefault (viewReplay replay)

        Nothing ->
            H.div [] []


createComment : Model -> List (H.Html Msg)
createComment model =
    case model.session.user |> Maybe.map .token of
        Just token ->
            let
                addCurVar =
                    model.replay
                        |> Maybe.Extra.filter (R.isInMainVar >> not)
                        |> Maybe.map (always [ H.button [ HA.class "add-current-variation", HE.onClick CommentAddVariation ] [ H.text "Add Current Variation" ] ])
                        |> Maybe.withDefault []

                submit =
                    [ H.button
                        [ HA.class "submit-comment", HE.onClick CreateComment ]
                        [ H.text "Create Comment" ]
                    , H.div [ HA.class "clear" ] []
                    ]

                ( postButton, cls ) =
                    if hasWipComment model then
                        ( addCurVar ++ submit, "focus" )

                    else
                        ( [], "" )

                currentPos =
                    case model.view of
                        ViewWipComment m ->
                            Just m

                        _ ->
                            Nothing
            in
            [ H.div [ HA.class "wip-comment" ]
                (C.viewCommentParts
                    (\m -> ViewWipComment m |> Jump)
                    currentPos
                    (getCommentParts model)
                )
            , H.textarea
                [ HA.class cls
                , HA.placeholder "Add your comment"
                , HA.value model.wipComment
                , HE.onFocus CommentFocus
                , HE.onInput CommentEdit
                , HE.onBlur CommentBlur
                ]
                []
            ]
                ++ postButton

        Nothing ->
            []


treeView : Maybe R.Replay -> H.Html Msg
treeView replay =
    let
        treeLayout : List (List (Maybe ( Replay.TreeLayout.Pos, G.Move, Bool )))
        treeLayout =
            replay
                |> Maybe.map .gameTree
                |> Maybe.map (Replay.TreeLayout.getTreeLayout 13 5)
                |> Maybe.withDefault []

        viewCell : Maybe ( a, G.Move, Bool ) -> H.Html msg
        viewCell maybeCell =
            case maybeCell of
                Just ( _, move, current ) ->
                    let
                        color =
                            if current then
                                "#CB5"

                            else
                                "#CCC"
                    in
                    H.td [] [ R.viewMoveHtml color move ]

                Nothing ->
                    H.td [] []
    in
    H.table [ HA.class "tree-view" ]
        (List.map (\r -> H.tr [] (List.map viewCell r)) treeLayout)


sideView : Model -> List (H.Html Msg)
sideView model =
    let
        moveNum : Int
        moveNum =
            model.replay
                |> Maybe.map R.currentMoveNumber
                |> Maybe.withDefault 0

        navMidItem =
            case model.view of
                ViewReplay ->
                    [ H.span [ HA.class "game-highlight" ] [ H.text <| String.fromInt moveNum ] ]

                _ ->
                    [ H.button
                        [ HA.class "game-clickable", HE.onClick <| Jump ViewReplay ]
                        [ H.text "review" ]
                    ]

        gameNav : H.Html Msg
        gameNav =
            H.div [ HA.class "pure-g game-nav" ]
                [ H.div [ HA.class "pure-u-1-5" ]
                    [ H.button [ HE.onClick Start ] [ start ] ]
                , H.div [ HA.class "pure-u-1-5" ]
                    [ H.button [ HE.onClick Backward ] [ backward ] ]
                , H.div [ HA.class "pure-u-1-5" ]
                    [ H.div [ HA.class "mid-item" ] navMidItem ]
                , H.div [ HA.class "pure-u-1-5" ]
                    [ H.button [ HE.onClick Forward ] [ forward ] ]
                , H.div [ HA.class "pure-u-1-5" ]
                    [ H.button [ HE.onClick End ] [ end ] ]
                ]
    in
    [ H.div [ HA.class "game-info" ] [ gameNav, treeView model.replay ]
    , H.div [ HA.class "comments" ]
        ((H.div [ HA.class "create-comment" ] <| createComment model)
            :: C.view
                (\c m -> ViewComment c m |> Jump)
                (currentComment model.view)
                model.comments
        )
    , H.div [ HA.class "message" ] [ H.text model.message ]
    ]


view : Model -> Page Msg
view model =
    let
        gameName =
            case model.replay of
                Just { record } ->
                    G.recordName record

                Nothing ->
                    "Loading"

        extraClass =
            model.replay
                |> Maybe.Extra.filter (\r -> r.record.game == G.Hex)
                |> Maybe.map (always "wider")
                |> Maybe.withDefault "narrower"
    in
    { title = gameName ++ " - Game Comment"
    , extraClass = extraClass
    , content = [ boardView model ]
    , sidebar = sideView model
    }
