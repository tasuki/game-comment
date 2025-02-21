{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module WebServerSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, (.:), Value(String))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Method as Method
import Network.Wai.Test (simpleBody)
import Test.Hspec (Spec, Expectation, describe, it, shouldBe, hspec)
import Test.Hspec.Wai (WaiSession, matchStatus, request, get, put, post, with, shouldRespondWith)
import Text.RawString.QQ

import qualified ApiResources as Res
import qualified ApiResources as Cmt (Comment(username, comment))
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
            post "/users" [r|{"username": "new", "password": "pass", "favorite": "twixt"}|]
                `shouldRespondWith` 200
        it "fails when captcha is missing" $ do
            post "/users" [r|{"username": "new", "password": "pass"}|]
                `shouldRespondWith` 400
        it "fails when captcha is wrong" $ do
            post "/users" [r|{"username": "new", "password": "pass", "favorite": "chess"}|]
                `shouldRespondWith` 400
        it "fails when username already exists" $ do
            post "/users" [r|{"username": "new", "password": "pass", "favorite": "twixt"}|]
                `shouldRespondWith` 200
            post "/users" [r|{"username": "new", "password": "pass", "favorite": "twixt"}|]
                `shouldRespondWith` 409

    describe "POST /sessions" $ do
        it "can create a session" $ do
            post "/users" [r|{"username": "user", "password": "pass", "favorite": "twixt"}|]
            post "/sessions" [r|{"username": "user", "password": "pass"}|]
                `shouldRespondWith` 200
        it "does not create a session with wrong password" $ do
            post "/users" [r|{"username": "user", "password": "pass", "favorite": "twixt"}|]
            post "/sessions" [r|{"username": "user", "password": "pass1"}|]
                `shouldRespondWith` 401

    describe "POST /user/password" $ do
        it "can update a password" $ do
            authHeader <- setupUserSession
            request Method.methodPost "/user/password" authHeader [r|{"password": "newpass"}|]
                `shouldRespondWith` 200
            post "/sessions" [r|{"username": "user", "password": "pass"}|]
                `shouldRespondWith` 401
            post "/sessions" [r|{"username": "user", "password": "newpass"}|]
                `shouldRespondWith` 200

    describe "PUT/GET /games/here/*" $ do
        it "can't fetch an inexistent game" $ do
            get "/games/here/gameid" `shouldRespondWith` 404
        it "can create a game and fetch it" $ do
            authHeader <- setupUserSession
            request Method.methodPut "/games/here/gameid" authHeader [r|thisisthegamerecord|]
                `shouldRespondWith` 200
            get "/games/here/gameid"
                `shouldRespondWith` [r|thisisthegamerecord|] {matchStatus = 200}

    describe "POST/GET /games/here/*/comments" $ do
        it "doesn't return any comments for an inexistent game" $ do
            get "/games/here/gameid/comments"
                `shouldRespondWith` [r|[]|] {matchStatus = 200}
        it "lets one create/read comments" $ do
            authHeader <- setupUserSession
            request Method.methodPut "/games/here/gameid" authHeader [r|thisisthegamerecord|]
            request Method.methodPost "/games/here/gameid/comments" authHeader [r|{"comment": "first!"}|]
            response <- get "/games/here/gameid/comments"
            let comments = decodeComments (simpleBody response)
            case comments of
                Just (cmnt : _) -> do
                    liftIO $ (Cmt.username cmnt) `shouldBe` "user"
                    liftIO $ (Cmt.comment cmnt) `shouldBe` "first!"
                Just [] -> do
                    error "No comments :scream:"
                Nothing ->
                    error "Failed to decode JSON into comments"

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

setupUserSession :: WaiSession st [Header.Header]
setupUserSession = do
    _ <- post "/users" [r|{"username": "user", "password": "pass", "favorite": "twixt"}|]
    response <- post "/sessions" [r|{"username": "user", "password": "pass"}|]
    return $ getAuthHeader (simpleBody response)

assertJsonField :: BL.ByteString -> T.Text -> (T.Text -> Expectation) -> Expectation
assertJsonField body field assertion =
    case decode body >>= parseMaybe (.: field) of
        Just (String value) -> assertion value
        Just _  -> error $ "Field " ++ show field ++ " is not a string."
        Nothing -> error $ "Field " ++ show field ++ " not found in response."

decodeComments :: BL.ByteString -> Maybe [Res.Comment]
decodeComments body = decode body :: Maybe [Res.Comment]
