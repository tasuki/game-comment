module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as HA
import Page
import Page.Game
import Page.Home
import Route
import Session exposing (Session)
import Url
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
    | Game Page.Game.Model


type alias Model =
    { key : Nav.Key
    , page : Page
    , message : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        session =
            { navKey = key }
    in
    changeRouteTo url
        { key = key
        , page = NotFound session
        , message = ""
        }



-- UPDATE


type Msg
    = Noop
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | HomeMsg Page.Home.Msg
    | GameMsg Page.Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.page ) of
        ( Noop, _ ) ->
            ( model, Cmd.none )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (getSession model.page).navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( UrlChanged url, _ ) ->
            changeRouteTo url model

        ( HomeMsg msg, Home m ) ->
            Page.Home.update msg m |> updateWith Home HomeMsg model

        ( GameMsg msg, Game m ) ->
            Page.Game.update msg m |> updateWith Game GameMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toPage toMsg model ( subModel, subCmd ) =
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

        Game m ->
            m.session


changeRouteTo : Url.Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        session =
            getSession model.page
    in
    case Url.Parser.parse Route.parser url of
        Just Route.Home ->
            Page.Home.init session
                |> updateWith Home HomeMsg model

        Just (Route.EmptyGame game size) ->
            Page.Game.initEmpty game size session
                |> updateWith Game GameMsg model

        Just (Route.LittleGolemGame lgId) ->
            Page.Game.initLg lgId session
                |> updateWith Game GameMsg model

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

        Game m ->
            Page.viewPage GameMsg (Page.Game.view m)

        NotFound _ ->
            { title = "Game Comment"
            , body = [ H.div [ HA.class "limit-width" ] [ H.h2 [] [ H.text "We haven't found it!" ] ] ]
            }
