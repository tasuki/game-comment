{-# LANGUAGE OverloadedStrings #-}

import Config (allowOrigin)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Lazy (Text)
import qualified Database.SQLite.Simple as SQL
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as Status
import qualified Web.Scotty as S

import qualified ApiResources as API
import qualified Database as DB
import qualified Utils as U

fetchGameRecord :: String -> IO (Status.Status, L8.ByteString)
fetchGameRecord gameId = do
    let gameUrl = "https://www.littlegolem.net/servlet/sgf/" <> gameId <> "/game.sgf"
    manager <- HTTP.newManager TLS.tlsManagerSettings
    request <- HTTP.parseRequest gameUrl
    response <- HTTP.httpLbs request manager
    return (HTTP.responseStatus response, HTTP.responseBody response)

jsonMsg :: Text -> S.ActionM ()
jsonMsg msg = S.json $ object [ "msg" .= (msg :: Text) ]

maybeSaveRecord :: SQL.Connection -> Status.Status -> Text -> Int -> Text -> IO (DB.SqlResult ())
maybeSaveRecord conn responseStatus source gameId sgf =
    if responseStatus == Status.status200
        then DB.saveRecord conn source gameId sgf
        else return DB.OtherError

main :: IO ()
main = do
    conn <- DB.open "game-comment.sqlite3"
    S.scotty 6483 $ do
        S.get "/games/lg/:gameId" $ do
            gameId <- S.param "gameId"
            (responseStatus, record) <- liftIO $ fetchGameRecord gameId
            _ <- liftIO $ maybeSaveRecord conn responseStatus "lg" 123 (U.lbsToLazyText record) -- TODO 123 !!!!!!!
            S.status responseStatus -- we don't mind too bad if this is off
            S.setHeader "Content-Type" "application/sgf; charset=iso-8859-1"
            S.setHeader "Access-Control-Allow-Origin" allowOrigin
            S.raw record

        S.post "/users" $ do
            user <- S.jsonData :: S.ActionM API.CreateUser
            creationResult <- liftIO $ DB.createUser conn user
            case creationResult of
                DB.Success () -> jsonMsg "User created successfully"
                DB.ConstraintError -> S.status Status.status409 >> jsonMsg "Username already exists"
                _ -> S.status Status.status500 >> jsonMsg "Unknown error"

        S.post "/sessions" $ do
            user <- S.jsonData :: S.ActionM API.CreateSession
            creationResult <- liftIO $ DB.authenticateUser conn user
            case creationResult of
                DB.Success True -> jsonMsg "Authenticated"
                DB.Success False -> jsonMsg "Username and password don't match"
                _ -> S.status Status.status500 >> jsonMsg "Unknown error"
