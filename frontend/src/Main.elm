port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Json.Encode as E
import LocalState exposing (LocalState)
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
        , update = updateWithStorage
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


port setStorage : E.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( model, cmds ) =
            update msg oldModel

        localState : E.Value
        localState =
            LocalState.encode { showFullMenu = model.showFullMenu, replays = model.replays }
    in
    ( model
    , Cmd.batch [ setStorage localState, cmds ]
    )



-- MODEL


type Page
    = NotFound Session
    | Home Page.Home.Model
    | Help Page.Help.Model
    | Game Page.Game.Model


type alias Model =
    { key : Nav.Key
    , showFullMenu : Bool
    , page : Page
    , message : String
    , currentUrl : Url
    , replays : Dict String R.Replay
    }


emptyModel : Nav.Key -> Session -> Url -> Model
emptyModel key session url =
    { key = key
    , showFullMenu = True
    , page = NotFound session
    , message = ""
    , currentUrl = url
    , replays = Dict.empty
    }


modelFromLocalState : Model -> LocalState -> Model
modelFromLocalState empty ls =
    { empty | showFullMenu = ls.showFullMenu, replays = ls.replays }


init : E.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init jsonValue url key =
    let
        session =
            { navKey = key }

        model =
            emptyModel key session url

        maybeLocalState =
            LocalState.decode jsonValue |> Result.toMaybe
    in
    changeRouteTo url <|
        Maybe.withDefault model
            (Maybe.map (modelFromLocalState model) maybeLocalState)



-- UPDATE


type Msg
    = Noop
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | HomeMsg Page.Home.Msg
    | HelpMsg Page.Help.Msg
    | GameMsg Page.Game.Msg
    | ToggleMenu
    | CloseGame String


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

        ( ToggleMenu, _ ) ->
            ( { model | showFullMenu = not model.showFullMenu }, Cmd.none )

        ( CloseGame url, _ ) ->
            closeGame url model

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


closeGame : String -> Model -> ( Model, Cmd Msg )
closeGame url model =
    let
        newReplays : Dict String R.Replay
        newReplays =
            Dict.remove url model.replays

        cmd =
            if url == model.currentUrl.path then
                Nav.pushUrl model.key (Route.toUrl <| Route.Home)

            else
                Cmd.none
    in
    ( { model | replays = newReplays }, cmd )


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

        Just (Route.Game source id) ->
            maybeLoadReplay (Page.Game.initGame source id session)
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
        gamesTiles =
            Navigation.getGamesTiles CloseGame model.showFullMenu model.replays

        nav : List (H.Html Msg)
        nav =
            [ Navigation.getNavigationTiles ToggleMenu model.currentUrl gamesTiles ]
    in
    case model.page of
        Home m ->
            Page.viewPage HomeMsg (Page.Home.view m) nav

        Help m ->
            Page.viewPage HelpMsg (Page.Help.view m) nav

        Game m ->
            Page.viewPage GameMsg (Page.Game.view m) nav

        NotFound _ ->
            { title = "Game Comment"
            , body = [ H.div [ HA.class "narrower" ] [ H.h2 [] [ H.text "We haven't found it!" ] ] ]
            }
