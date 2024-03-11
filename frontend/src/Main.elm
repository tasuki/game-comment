module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
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
            let
                ( newModel, newCmd ) =
                    Page.Game.update msg m

                newReplays =
                    case newModel.replay of
                        Just replay ->
                            Dict.insert (Url.toString model.currentUrl) replay model.replays

                        _ ->
                            model.replays
            in
            updateWith { model | replays = newReplays } Game GameMsg ( newModel, newCmd )

        ( _, _ ) ->
            ( model, Cmd.none )


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

        maybeLoadReplay defaultModel =
            case Dict.get (Url.toString url) model.replays of
                Just replay ->
                    Page.Game.initPrevious replay (getSession model.page)

                Nothing ->
                    defaultModel
    in
    case Url.Parser.parse Route.parser url of
        Just Route.Home ->
            Page.Home.init session
                |> updateWith model Home HomeMsg

        Just Route.Help ->
            Page.Help.init session
                |> updateWith model Help HelpMsg

        Just (Route.EmptyGame game size) ->
            maybeLoadReplay (Page.Game.initEmpty game size session)
                |> updateWith model Game GameMsg

        Just (Route.LittleGolemGame lgId) ->
            maybeLoadReplay (Page.Game.initLg lgId session)
                |> updateWith model Game GameMsg

        Nothing ->
            ( { model | page = NotFound session }
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
    case model.page of
        Home m ->
            Page.viewPage HomeMsg (Page.Home.view m)

        Help _ ->
            Page.viewPage HelpMsg Page.Help.view

        Game m ->
            Page.viewPage GameMsg (Page.Game.view m)

        NotFound _ ->
            { title = "Game Comment"
            , body = [ H.div [ HA.class "limit-width" ] [ H.h2 [] [ H.text "We haven't found it!" ] ] ]
            }
