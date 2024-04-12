module Page.Game exposing (..)

import ApiClient as AC
import Browser.Events
import Game.Go
import Game.Hex
import Game.ToroidGo
import Game.TwixT
import GameRecord as G
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Page exposing (Page)
import Replay as R
import Route
import Session exposing (Session)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import Url exposing (Url)
import Url.Parser



-- MODEL


type alias Model =
    { session : Session
    , replay : Maybe R.Replay
    , message : String
    }


sidebarMsg : String
sidebarMsg =
    "Use the left/right keys to explore the game."


initEmpty : G.Game -> Int -> Session -> ( Model, Cmd Msg )
initEmpty game size session =
    ( { session = session
      , replay = Just <| R.emptyReplay <| G.empty game size
      , message = sidebarMsg
      }
    , Cmd.none
    )


initLg : String -> Session -> ( Model, Cmd Msg )
initLg lgId session =
    ( { session = session
      , replay = Nothing
      , message = sidebarMsg
      }
    , AC.getLittleGolemSgf Fetched lgId
    )


initPrevious : R.Replay -> Session -> ( Model, Cmd Msg )
initPrevious replay session =
    ( { session = session
      , replay = Just replay
      , message = ""
      }
    , Task.succeed Reload |> Task.perform identity
    )



-- UPDATE


type Msg
    = Noop
    | Reload
    | Fetched AC.SgfResult
    | Play G.Coords
    | Forward
    | Backward
    | Start
    | End
    | Jump R.LookAt


update : Msg -> Model -> Url -> ( Model, Cmd Msg )
update msg model currentUrl =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Reload ->
            case Url.Parser.parse Route.parser currentUrl of
                Just (Route.LittleGolemGame lgId) ->
                    ( model, AC.getLittleGolemSgf Fetched lgId )

                _ ->
                    ( model, Cmd.none )

        Fetched result ->
            case result of
                Ok record ->
                    ( { model | replay = Just <| R.withRecord record model.replay }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | message = "Could not load game: [ " ++ error ++ " ]" }
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

        Jump lookAt ->
            ( { model | replay = Maybe.map (R.jump lookAt) model.replay }, Cmd.none )

        Play coords ->
            let
                isMoveLegal replay =
                    case replay.record.game of
                        G.ToroidGo ->
                            Game.ToroidGo.isLegal replay coords

                        G.Go ->
                            Game.Go.isLegal replay coords

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
subscriptions m =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map keydown <| D.field "key" D.string)
        ]


keydown : String -> Msg
keydown keycode =
    case keycode of
        "ArrowLeft" ->
            Backward

        "ArrowRight" ->
            Forward

        "k" ->
            Backward

        "j" ->
            Forward

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
                    replay.lookingAt.move

                Nothing ->
                    0

        gameNav : H.Html Msg
        gameNav =
            H.div [ HA.class "pure-g game-nav" ]
                [ H.div [ HA.class "pure-u-1-5" ] [ H.button [ HE.onClick Start ] [ start ] ]
                , H.div [ HA.class "pure-u-1-5" ] [ H.button [ HE.onClick Backward ] [ backward ] ]
                , H.div [ HA.class "pure-u-1-5" ] [ H.text <| String.fromInt moveNum ]
                , H.div [ HA.class "pure-u-1-5" ] [ H.button [ HE.onClick Forward ] [ forward ] ]
                , H.div [ HA.class "pure-u-1-5" ] [ H.button [ HE.onClick End ] [ end ] ]
                ]

        gameInfo : List (H.Html Msg)
        gameInfo =
            case model.replay of
                Just replay ->
                    R.view Jump gameNav replay

                Nothing ->
                    []
    in
    [ H.div [ HA.class "game-info" ] gameInfo
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
            case model.replay of
                Just { record } ->
                    if record.game == G.Hex then
                        "wider"

                    else
                        "narrower"

                Nothing ->
                    "narrower"
    in
    { title = gameName ++ " - Game Comment"
    , extraClass = extraClass
    , content = [ boardView model ]
    , sidebar = sideView model
    }
