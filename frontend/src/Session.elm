module Session exposing (..)

import Browser.Navigation as Nav
import User



type alias Session =
    { navKey : Nav.Key
    , user : Maybe User.User
    }
