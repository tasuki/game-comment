module Page.Game exposing (..)

import ApiClient as AC
import Browser.Events
import Colours
import Comments as C
import Game.Go
import Game.Hex
import Game.ToroidGo
import Game.TwixT
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Maybe.Extra
import Page exposing (Page)
import Replay as R
import Route
import Session exposing (Session)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import Url exposing (Url)



-- MODEL


type Viewing
    = ViewReplay
    | ViewComment Int Int


type alias Model =
    { session : Session
    , viewing : Viewing
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
      , viewing = ViewReplay
      , replay = Just <| R.emptyReplay <| G.empty game size
      , comments = []
      , message = sidebarMsg
      }
    , Cmd.none
    )


initGame : G.GameSource -> Session -> ( Model, Cmd Msg )
initGame gameSource session =
    ( { session = session
      , viewing = ViewReplay
      , replay = Nothing
      , comments = []
      , message = sidebarMsg
      }
    , AC.getSgf Fetched gameSource
    )


initPrevious : R.Replay -> Session -> ( Model, Cmd Msg )
initPrevious replay session =
    ( { session = session
      , viewing = ViewReplay -- TODO preserve previous view?
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
    | Play G.Coords
    | Forward
    | Backward
    | Start
    | End
    | JumpComment Int Int
    | PrevVariation
    | NextVariation
    | CutVariation


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

        Start ->
            ( { model | replay = Maybe.map R.start model.replay }, Cmd.none )

        Backward ->
            ( { model | replay = Maybe.map R.prev model.replay }, Cmd.none )

        Forward ->
            ( { model | replay = Maybe.map R.next model.replay }, Cmd.none )

        End ->
            ( { model | replay = Maybe.map R.end model.replay }, Cmd.none )

        PrevVariation ->
            ( { model | replay = Maybe.map R.prevVariation model.replay }, Cmd.none )

        NextVariation ->
            ( { model | replay = Maybe.map R.nextVariation model.replay }, Cmd.none )

        CutVariation ->
            -- TODO
            --( { model | replay = Maybe.map R.cutVariation model.replay }, Cmd.none )
            ( model, Cmd.none )

        JumpComment comment move ->
            ( { model | viewing = ViewComment comment move }, Cmd.none )

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
            case Maybe.map isMoveLegal model.replay of
                Just True ->
                    ( { model | replay = Maybe.map (R.playCoords coords) model.replay }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



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
            PrevVariation

        "ArrowDown" ->
            NextVariation

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
    case model.replay of
        Just replay ->
            let
                specificView : R.Replay -> (G.Coords -> msg) -> Svg msg
                specificView =
                    case replay.record.game of
                        G.TwixT ->
                            Game.TwixT.view

                        G.ToroidGo ->
                            Game.ToroidGo.view

                        G.Go ->
                            Game.Go.view

                        G.Hex ->
                            Game.Hex.view
            in
            specificView replay Play

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

        gameNav : H.Html Msg
        gameNav =
            H.div [ HA.class "pure-g game-nav" ]
                [ H.div [ HA.class "pure-u-1-5" ] [ H.button [ HE.onClick Start ] [ start ] ]
                , H.div [ HA.class "pure-u-1-5" ] [ H.button [ HE.onClick Backward ] [ backward ] ]
                , H.div [ HA.class "pure-u-1-5" ]
                    [ H.div
                        [ HA.style "padding" "7px 10px" ]
                        [ H.text <| String.fromInt moveNum ]
                    ]
                , H.div [ HA.class "pure-u-1-5" ] [ H.button [ HE.onClick Forward ] [ forward ] ]
                , H.div [ HA.class "pure-u-1-5" ] [ H.button [ HE.onClick End ] [ end ] ]
                ]
    in
    [ H.div [ HA.class "game-info" ] [ gameNav ]
    , H.div [ HA.class "comments" ] (C.view JumpComment model.comments)
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
