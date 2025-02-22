module Page.Login exposing (..)

import ApiClient as AC
import Browser.Navigation as Nav
import GameHelpers as GH
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Page exposing (Page)
import Random
import Route
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
    ( { session = session
      , message = ""
      , createName = ""
      , favorite = ""
      , loginName = ""
      , loginPass = ""
      }
    , Cmd.none
    )


initLogin : Session -> String -> String -> ( Model, Cmd Msg )
initLogin session user pass =
    let
        cmd =
            case session.user of
                Just _ ->
                    Cmd.none

                Nothing ->
                    AC.createSession LoggedIn
                        { username = user
                        , password = pass
                        }
    in
    ( { session = session
      , message = ""
      , createName = ""
      , favorite = ""
      , loginName = user
      , loginPass = pass
      }
    , cmd
    )



-- UPDATE


type Msg
    = EnterCreateName String
    | EnterFavorite String
    | CreateAccount
    | DoCreateAccount String
    | AccountCreated String AC.UserCreatedResult
    | EnterLoginName String
    | EnterLoginPass String
    | LogIn
    | LoggedIn AC.SessionResult


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
            , AC.createUser (AccountCreated password)
                { username = model.createName
                , password = password
                , favorite = model.favorite
                , email = Nothing
                }
            )

        AccountCreated password cr ->
            case cr of
                Ok _ ->
                    update LogIn
                        { model
                            | loginName = model.createName
                            , loginPass = password
                            , message = "Account created!"
                        }

                Err err ->
                    ( { model | message = "Could not create user: [ " ++ err.msg ++ " ]" }
                    , Cmd.none
                    )

        EnterLoginName name ->
            ( { model | loginName = name }, Cmd.none )

        EnterLoginPass pass ->
            ( { model | loginPass = pass }, Cmd.none )

        LogIn ->
            ( model
            , AC.createSession LoggedIn
                { username = model.loginName
                , password = model.loginPass
                }
            )

        LoggedIn sessionResult ->
            case sessionResult of
                Ok sessionData ->
                    let
                        ( maybeUser, messge ) =
                            case User.sessionDataToUser sessionData of
                                Ok user ->
                                    ( Just user, "" )

                                Err err ->
                                    ( Nothing, err )

                        ns =
                            model.session
                    in
                    ( { model | session = { ns | user = maybeUser }, message = "Logged in!" }
                    , Nav.pushUrl model.session.navKey <| Route.toUrl (Route.LoggedIn model.loginName model.loginPass)
                    )

                Err err ->
                    ( { model | message = "Could not log in: [ " ++ err.msg ++ " ]" }
                    , Cmd.none
                    )



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
            , HE.onInput EnterLoginName
            , GH.onEnter LogIn
            ]
            []
        ]
    , H.div []
        [ H.input
            [ HA.type_ "text"
            , HA.placeholder "Password"
            , HE.onInput EnterLoginPass
            , GH.onEnter LogIn
            ]
            []
        ]
    , H.div []
        [ H.button
            [ HE.onClick LogIn ]
            [ H.text "Log in" ]
        ]
    , H.br [] []
    , H.div [] [ H.text model.message ]
    ]


view : Model -> Page Msg
view model =
    { title = "Game Comment - User Account"
    , extraClass = "user narrower"
    , content = forms model
    , sidebar = Page.sideHelp
    }
