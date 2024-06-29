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
import Replay as R
import Route
import Session exposing (Session)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import Url exposing (Url)



-- Viewing


type View
    = ViewReplay
    | ViewComment Int Int


viewPrevious : View -> View
viewPrevious v =
    case v of
        ViewReplay ->
            v

        ViewComment comment _ ->
            if comment > 0 then
                ViewComment (comment - 1) 0

            else
                ViewReplay


viewNext : Int -> View -> View
viewNext comments v =
    case v of
        ViewReplay ->
            ViewComment 0 0

        ViewComment comment _ ->
            if comment < comments - 1 then
                ViewComment (comment + 1) 0

            else
                v


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
    , replay : Maybe R.Replay
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
      , replay = Just <| R.emptyReplay <| G.empty game size
      , comments = []
      , message = sidebarMsg
      }
    , Cmd.none
    )


initGame : G.GameSource -> Session -> ( Model, Cmd Msg )
initGame gameSource session =
    ( { session = session
      , view = ViewReplay
      , replay = Nothing
      , comments = []
      , message = sidebarMsg
      }
    , AC.getSgf Fetched gameSource
    )


initPrevious : R.Replay -> Session -> ( Model, Cmd Msg )
initPrevious replay session =
    ( { session = session
      , view = ViewReplay -- TODO preserve previous view?
      , replay = Just replay
      , comments = [] -- TODO definitely preserve comments!
      , message = ""
      }
    , Task.succeed Reload |> Task.perform identity
    )



-- UPDATE


type Msg
    = Noop
    | Reload
    | Fetched AC.SgfResult
    | FetchedComments AC.CommentsResult
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


onlyInReplay : Model -> Model -> ( Model, Cmd msg )
onlyInReplay model new =
    case model.view of
        ViewReplay ->
            ( new, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> Url -> ( Model, Cmd Msg )
update msg model currentUrl =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Reload ->
            case Route.parse currentUrl of
                Just (Route.Game source lgId) ->
                    let
                        gameSource =
                            G.GameSource source lgId
                    in
                    ( model
                    , AC.getSgf Fetched gameSource
                    )

                _ ->
                    ( model, Cmd.none )

        Fetched result ->
            case result of
                Ok record ->
                    let
                        shouldUpdate =
                            model.replay
                                |> Maybe.map (\r -> r.record.source == record.source)
                                |> Maybe.withDefault True
                    in
                    if shouldUpdate then
                        ( { model | replay = Just <| R.withRecord record model.replay }
                        , AC.getComments FetchedComments record.source
                        )

                    else
                        ( model, Cmd.none )

                Err error ->
                    ( { model | message = "Could not load game: [ " ++ error ++ " ]" }
                    , Cmd.none
                    )

        FetchedComments result ->
            case result of
                Ok comments ->
                    case model.replay of
                        Just replay ->
                            ( { model | comments = List.map (C.getComment replay.record) comments }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | message = "No replay, so not showing comments..." }
                            , Cmd.none
                            )

                Err error ->
                    ( { model | message = "Could not load comments: [ " ++ error ++ " ]" }
                    , Cmd.none
                    )

        PrevView ->
            ( { model | view = viewPrevious model.view }, Cmd.none )

        NextView ->
            ( { model | view = viewNext (List.length model.comments) model.view }, Cmd.none )

        Jump jumpView ->
            ( { model | view = jumpView }, Cmd.none )

        Backward ->
            case model.view of
                ViewReplay ->
                    ( { model | replay = Maybe.map R.prev model.replay }, Cmd.none )

                ViewComment cPos clickable ->
                    ( { model | view = ViewComment cPos (C.prevClickable clickable) }, Cmd.none )

        Forward ->
            case model.view of
                ViewReplay ->
                    ( { model | replay = Maybe.map R.next model.replay }, Cmd.none )

                ViewComment cPos clickable ->
                    case List.Extra.getAt cPos model.comments of
                        Just comment ->
                            ( { model | view = ViewComment cPos (C.nextClickable comment clickable) }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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
    in
    case model.replay of
        Just replay ->
            case model.view of
                ViewReplay ->
                    specificView replay
                        replay.record.size
                        (R.currentMoves replay)
                        Nothing
                        (R.currentColour replay)
                        (R.children replay)
                        (R.lastPlayed replay)
                        (R.onMove replay.record.game replay)
                        Play

                ViewComment commentPos clickablePos ->
                    C.getClickable commentPos clickablePos model.comments
                        |> Maybe.map (viewComment replay)
                        |> Maybe.withDefault
                            (H.text "This branch shouldn't even exist...")

        Nothing ->
            H.div [] []


sideView : Model -> List (H.Html Msg)
sideView model =
    let
        moveNum : Int
        moveNum =
            case model.replay of
                Just replay ->
                    R.currentMoveNumber replay

                Nothing ->
                    0

        navMidItem =
            case model.view of
                ViewReplay ->
                    [ H.text <| String.fromInt moveNum ]

                _ ->
                    [ H.button
                        [ HA.id "back-review", HE.onClick <| Jump ViewReplay ]
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
                    [ H.div [ HA.style "padding" "7px 10px" ] navMidItem ]
                , H.div [ HA.class "pure-u-1-5" ]
                    [ H.button [ HE.onClick Forward ] [ forward ] ]
                , H.div [ HA.class "pure-u-1-5" ]
                    [ H.button [ HE.onClick End ] [ end ] ]
                ]
    in
    [ H.div [ HA.class "game-info" ] [ gameNav ]
    , H.div [ HA.class "comments" ]
        (C.view
            (\c m -> Jump <| ViewComment c m)
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
