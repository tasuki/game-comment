module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Navigation
import Page
import Page.Game
import Page.Help
import Page.Home
import Replay as R
import Route
import Session exposing (Session)
import Url exposing (Url)
import Url.Parser


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type Page
    = NotFound Session
    | Home Page.Home.Model
    | Help Page.Help.Model
    | Game Page.Game.Model


type alias Model =
    { key : Nav.Key
    , page : Page
    , message : String
    , currentUrl : Url
    , replays : Dict String R.Replay
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        session =
            { navKey = key }
    in
    changeRouteTo url
        { key = key
        , page = NotFound session
        , message = ""
        , currentUrl = url
        , replays = Dict.empty
        }



-- UPDATE


type Msg
    = Noop
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | HomeMsg Page.Home.Msg
    | HelpMsg Page.Help.Msg
    | GameMsg Page.Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.page ) of
        ( Noop, _ ) ->
            ( model, Cmd.none )

        ( LinkClicked urlRequest, _ ) ->
            handleLinkClick urlRequest model

        ( UrlChanged url, _ ) ->
            changeRouteTo url model

        ( HomeMsg msg, Home m ) ->
            Page.Home.update msg m |> updateWith model Home HomeMsg

        ( GameMsg msg, Game m ) ->
            updateGamePage model msg m

        ( _, _ ) ->
            ( model, Cmd.none )


updateGamePage : Model -> Page.Game.Msg -> Page.Game.Model -> ( Model, Cmd Msg )
updateGamePage model msg m =
    let
        ( newModel, newCmd ) =
            Page.Game.update msg m model.currentUrl

        newReplays =
            case newModel.replay of
                Just replay ->
                    Dict.insert model.currentUrl.path replay model.replays

                _ ->
                    model.replays
    in
    updateWith { model | replays = newReplays } Game GameMsg ( newModel, newCmd )


updateWith : Model -> (subModel -> Page) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith model toPage toMsg ( subModel, subCmd ) =
    ( { model | page = toPage subModel }
    , Cmd.map toMsg subCmd
    )


getSession : Page -> Session
getSession page =
    case page of
        NotFound session ->
            session

        Home m ->
            m.session

        Help m ->
            m.session

        Game m ->
            m.session


handleLinkClick : Browser.UrlRequest -> Model -> ( Model, Cmd msg )
handleLinkClick urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model, Nav.pushUrl (getSession model.page).navKey (Url.toString url) )

        Browser.External href ->
            ( model, Nav.load href )


changeRouteTo : Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        session =
            getSession model.page

        newModel =
            { model | currentUrl = url }

        maybeLoadReplay defaultModel =
            case Dict.get url.path model.replays of
                Just replay ->
                    Page.Game.initPrevious replay (getSession model.page)

                Nothing ->
                    defaultModel
    in
    case Url.Parser.parse Route.parser url of
        Just Route.Home ->
            Page.Home.init session
                |> updateWith newModel Home HomeMsg

        Just Route.Help ->
            Page.Help.init session
                |> updateWith newModel Help HelpMsg

        Just (Route.EmptyGame game size) ->
            maybeLoadReplay (Page.Game.initEmpty game size session)
                |> updateWith newModel Game GameMsg

        Just (Route.LittleGolemGame lgId) ->
            maybeLoadReplay (Page.Game.initLg lgId session)
                |> updateWith newModel Game GameMsg

        Nothing ->
            ( { newModel | page = NotFound session }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Game m ->
            Sub.map GameMsg (Page.Game.subscriptions m)

        _ ->
            Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        nav : List (H.Html msg)
        nav =
            Navigation.getNavigationTiles model.currentUrl model.replays
    in
    case model.page of
        Home m ->
            Page.viewPage HomeMsg (Page.Home.view m nav)

        Help m ->
            Page.viewPage HelpMsg (Page.Help.view m nav)

        Game m ->
            Page.viewPage GameMsg (Page.Game.view m nav)

        NotFound _ ->
            { title = "Game Comment"
            , body = [ H.div [ HA.class "limit-width" ] [ H.h2 [] [ H.text "We haven't found it!" ] ] ]
            }
