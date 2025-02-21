module Page.User exposing (..)

import ApiClient as AC
import Expect exposing (pass)
import GameHelpers as GH
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Page exposing (Page)
import Random
import Session exposing (Session)
import User



-- MODEL


type alias Model =
    { session : Session
    , message : String
    , createName : String
    , favorite : String
    , loginName : String
    , loginPass : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, message = "", createName = "", favorite = "", loginName = "", loginPass = "" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EnterCreateName String
    | EnterFavorite String
    | CreateAccount
    | DoCreateAccount String
    | Created AC.CreatedResult
    | EnterLoginName String
    | EnterLoginPass String
    | LogIn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnterCreateName name ->
            ( { model | createName = name }, Cmd.none )

        EnterFavorite fav ->
            ( { model | favorite = fav }, Cmd.none )

        CreateAccount ->
            ( model, Random.generate DoCreateAccount <| User.passwordGen 15 )

        DoCreateAccount password ->
            ( model
            , AC.createUser Created
                { username = model.createName
                , password = password
                , favorite = model.favorite
                , email = Nothing
                }
            )

        Created cr ->
            case cr of
                Ok record ->
                    -- TODO
                    ( model, Cmd.none )

                Err err ->
                    ( { model | message = "Could not create user: [ " ++ err ++ " ]" }
                    , Cmd.none
                    )

        EnterLoginName name ->
            ( { model | loginName = name }, Cmd.none )

        EnterLoginPass pass ->
            ( { model | loginPass = pass }, Cmd.none )

        LogIn ->
            ( model, Cmd.none )



-- VIEW


forms : Model -> List (H.Html Msg)
forms model =
    [ H.p [] [ H.text "Create a user account:" ]
    , H.div []
        [ H.input
            [ HA.type_ "text"
            , HA.placeholder "Username"
            , HE.onInput EnterCreateName
            , GH.onEnter CreateAccount
            ]
            []
        ]
    , H.div []
        [ H.input
            [ HA.type_ "text"
            , HA.placeholder "Favourite game?"
            , HE.onInput EnterFavorite
            , GH.onEnter CreateAccount
            ]
            []
        , H.text "Pick one we have here please!"
        ]
    , H.div []
        [ H.button
            [ HE.onClick <| CreateAccount ]
            [ H.text "Create an account" ]
        ]
    , H.br [] []
    , H.h3 [] [ H.text "~~ OR ~~" ]
    , H.br [] []
    , H.p [] [ H.text "Log in:" ]
    , H.div []
        [ H.input
            [ HA.type_ "text"
            , HA.placeholder "Username"
            , GH.onEnter LogIn
            ]
            []
        ]
    , H.div []
        [ H.input
            [ HA.type_ "text"
            , HA.placeholder "Password"
            , GH.onEnter LogIn
            ]
            []
        ]
    , H.div []
        [ H.button
            [ HE.onClick LogIn ]
            [ H.text "Log in" ]
        ]
    ]


view : Model -> Page Msg
view model =
    { title = "Game Comment - User Account"
    , extraClass = "user narrower"
    , content = forms model
    , sidebar = Page.sideHelp
    }
