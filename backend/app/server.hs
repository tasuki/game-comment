{-# LANGUAGE OverloadedStrings #-}

import Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Lazy (Text)
import Database.SQLite.Simple
import Network.HTTP.Client (httpLbs, newManager, parseRequest, Response(responseStatus, responseBody))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Web.Scotty

import ApiResources
import Database
import Utils

fetchGameRecord :: String -> IO (Status, L8.ByteString)
fetchGameRecord gameId = do
    let gameUrl = "https://www.littlegolem.net/servlet/sgf/" <> gameId <> "/game.sgf"
    manager <- newManager tlsManagerSettings
    request <- parseRequest gameUrl
    response <- httpLbs request manager
    return (responseStatus response, responseBody response)

jsonMsg :: Text -> ActionM ()
jsonMsg msg = json $ object [ "msg" .= (msg :: Text) ]

maybeSaveRecord :: Connection -> Status -> Text -> Int -> Text -> IO (SqlResult ())
maybeSaveRecord conn responseStatus source gameId sgf =
    if responseStatus == status200
        then saveRecord conn source gameId sgf
        else return OtherError

main :: IO ()
main = do
    conn <- openDb "game-comment.sqlite3"
    scotty 6483 $ do
        get "/games/lg/:gameId" $ do
            gameId <- param "gameId"
            (responseStatus, record) <- liftIO $ fetchGameRecord gameId
            _ <- liftIO $ maybeSaveRecord conn responseStatus "lg" 123 (lbsToLazyText record) -- TODO 123 !!!!!!!
            status responseStatus -- we don't mind too bad if this is off
            setHeader "Content-Type" "application/sgf; charset=iso-8859-1"
            setHeader "Access-Control-Allow-Origin" allowOrigin
            raw record

        post "/users" $ do
            user <- jsonData :: ActionM CreateUser
            creationResult <- liftIO $ createUser conn user
            case creationResult of
                Success () -> jsonMsg "User created successfully"
                ConstraintError -> status status409 >> jsonMsg "Username already exists"
                _ -> status status500 >> jsonMsg "Unknown error"

        post "/sessions" $ do
            user <- jsonData :: ActionM CreateSession
            creationResult <- liftIO $ authenticateUser conn user
            case creationResult of
                Success True -> jsonMsg "Authenticated"
                Success False -> jsonMsg "Username and password don't match"
                _ -> status status500 >> jsonMsg "Unknown error"
