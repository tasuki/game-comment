{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module WebServerSpec where

import Test.Hspec
import Test.Hspec.Wai
import Text.RawString.QQ
import qualified Network.Wai as Wai

import qualified Database as DB
import qualified Env
import qualified Migrations
import qualified WebServer

main :: IO ()
main = hspec spec

spec :: Spec
spec = with setupApp $ do
    describe "POST /users" $ do
        it "creates a user successfully" $ do
            let userJson = [r|{"username": "user", "password": "pass"}|]
            let response = post "/users" userJson
            response `shouldRespondWith` 200

        it "fails when username already exists" $ do
            let userJson = [r|{"username": "user", "password": "pass123"}|]
            post "/users" userJson
            post "/users" userJson `shouldRespondWith` 409

setupApp :: IO Wai.Application
setupApp = do
    config <- Env.readEnvVars
    conn <- DB.open ":memory:"
    Migrations.migrate conn
    WebServer.waiApp config conn
