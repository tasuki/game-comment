{-# LANGUAGE OverloadedStrings #-}

module Auth where

import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Web.JWT as JWT

import qualified ApiResources as AR
import qualified ApiResources as UD (UserData(id, username))
import qualified Utils as U

createJwt :: String -> Integer -> AR.UserData -> TL.Text
createJwt secretKey nowTime userData =
    let
        cs = mempty
            { JWT.unregisteredClaims = JWT.ClaimsMap $ Map.fromList
                [ ("id", A.Number $ fromIntegral $ UD.id userData)
                , ("username", A.String $ U.lazyTextToText $ UD.username userData)
                ]
            , JWT.exp = JWT.numericDate $ fromIntegral $ nowTime + 60*60*24*365
            }
        key = JWT.hmacSecret (U.stringToText secretKey)
    in TL.fromStrict $ JWT.encodeSigned key mempty cs

verifyJwt :: String -> String -> Maybe AR.UserData
verifyJwt secretKey jwt =
    let key = JWT.hmacSecret (U.stringToText secretKey)
        decodedJwt = JWT.decodeAndVerifySignature key (U.stringToText jwt)
    in case decodedJwt of
        Nothing -> Nothing
        Just verifiedJwt ->
            let claims = JWT.claims verifiedJwt
                JWT.ClaimsMap unregistered = JWT.unregisteredClaims claims
                idClaim = Map.lookup "id" unregistered
                usernameClaim = Map.lookup "username" unregistered
            in case (idClaim, usernameClaim) of
                (Just (A.Number id), Just (A.String username)) ->
                    Just $ AR.UserData (floor id) (TL.fromStrict username)
                _ -> Nothing
