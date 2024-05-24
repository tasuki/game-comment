{-# LANGUAGE OverloadedStrings #-}

module Auth where

import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Web.JWT as JWT

import qualified ApiResources as AR
import qualified ApiResources as UD (UserData(id, username))
import qualified Utils as U

createJwt :: String -> Integer -> AR.UserData -> T.Text
createJwt secretKey nowTime userData =
    let
        cs = mempty
            { JWT.unregisteredClaims = JWT.ClaimsMap $ Map.fromList
                [ ("id", (A.Number $ fromIntegral $ UD.id userData))
                , ("username", (A.String $ U.lazyTextToText $ UD.username userData))
                ]
            , JWT.exp = JWT.numericDate $ fromIntegral $ nowTime + 60*60*24*365
            }
        key = JWT.hmacSecret (U.stringToText secretKey)
    in JWT.encodeSigned key mempty cs
