module Session exposing (..)

import Browser.Navigation as Nav
import User


type alias Session =
    { navKey : Nav.Key
    , user : Maybe User.User
    }


withToken : Maybe String -> Session -> Session
withToken maybeAuthToken session =
    case maybeAuthToken of
        Nothing ->
            session

        Just authToken ->
            { session
                | user =
                    Result.toMaybe <|
                        User.authTokenToUser authToken
            }
