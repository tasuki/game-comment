{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module WebServerSpec where

import Data.Aeson (decode, (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Method as Method
import Network.Wai.Test (simpleBody)
import Test.Hspec
import Test.Hspec.Wai
import Text.RawString.QQ

import Api (jsonData, jsonError, jsonMsg, onlySignedIn)
import qualified Database as DB
import qualified Env
import qualified Migrations
import qualified WebServer
import qualified ApiResources as Res

main :: IO ()
main = hspec spec

spec :: Spec
spec = with setupApp $ do
    describe "POST /users" $ do
        it "creates a user successfully" $ do
            let userJson = [r|{"username": "new", "password": "pass"}|]
            post "/users" userJson `shouldRespondWith` 200
        it "fails when username already exists" $ do
            post "/users" [r|{"username": "dupe", "password": "pass"}|] `shouldRespondWith` 200
            post "/users" [r|{"username": "dupe", "password": "pass"}|] `shouldRespondWith` 409

    describe "POST /sessions" $ do
        it "can create a session" $ do
            post "/users" [r|{"username": "user", "password": "pass"}|]
            post "/sessions" [r|{"username": "user", "password": "pass"}|] `shouldRespondWith` 200
        it "does not create a session with wrong password" $ do
            post "/users" [r|{"username": "user", "password": "pass"}|]
            post "/sessions" [r|{"username": "user", "password": "pass1"}|] `shouldRespondWith` 401

    describe "POST /user/password" $ do
        it "can update a password" $ do
            post "/users" [r|{"username": "user", "password": "pass"}|]
            response <- post "/sessions" [r|{"username": "user", "password": "pass"}|]
            let authHeader = getAuthHeader (simpleBody response)
            request Method.methodPost "/user/password" authHeader [r|{"password": "newpass"}|] `shouldRespondWith` 200
            post "/sessions" [r|{"username": "user", "password": "pass"}|] `shouldRespondWith` 401
            post "/sessions" [r|{"username": "user", "password": "newpass"}|] `shouldRespondWith` 200

setupApp :: IO Wai.Application
setupApp = do
    config <- Env.readEnvVars
    conn <- DB.open ":memory:"
    Migrations.migrate conn
    WebServer.waiApp config conn

getAuthHeader :: BL.ByteString -> [Header.Header]
getAuthHeader body = do
    case decode body >>= \obj -> parseMaybe (.: "authToken") obj of
        Just token -> [(Header.hAuthorization, "Bearer " <> TE.encodeUtf8 token)]
        Nothing -> []
