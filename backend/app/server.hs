{-# LANGUAGE OverloadedStrings #-}

import Config (allowOrigin)
import Control.Exception (try)
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

jsonMsg :: Text -> S.ActionM ()
jsonMsg msg = S.json $ object [ "msg" .= (msg :: Text) ]

type GameFetched = Either HTTP.HttpException (Status.Status, L8.ByteString)

fetchLittleGolemGameRecord :: Int -> IO GameFetched
fetchLittleGolemGameRecord gameId = do
    let gameUrl = "https://www.littlegolem.net/servlet/sgf/" <> (show gameId) <> "/game.sgf"
    try $ do
        manager <- HTTP.newManager TLS.tlsManagerSettings
        request <- HTTP.parseRequest gameUrl
        response <- HTTP.httpLbs request manager
        return (HTTP.responseStatus response, HTTP.responseBody response)

maybeSaveRecord :: SQL.Connection -> Text -> Int -> GameFetched -> IO (DB.SqlResult ())
maybeSaveRecord conn source gameId eitherResult =
    case eitherResult of
        Right (responseStatus, sgf) | responseStatus == Status.status200 ->
            DB.saveRecord conn source gameId (U.lbsToLazyText sgf)
        _ ->
            return $ DB.Success ()

getGame :: (Int -> IO GameFetched) -> SQL.Connection -> Text -> Int -> S.ActionM ()
getGame fetcher conn source gameId = do
    result <- liftIO $ fetcher gameId
    _ <- liftIO $ maybeSaveRecord conn source gameId result
    record <- liftIO $ DB.fetchRecord conn source gameId
    case record of
        DB.OtherError ->
            S.status Status.status404 >> jsonMsg "Game record not found"
        DB.Success record -> do
            S.status Status.status200
            S.setHeader "Content-Type" "application/sgf; charset=iso-8859-1"
            S.setHeader "Access-Control-Allow-Origin" allowOrigin
            S.raw $ U.lazyTextToLbs record

main :: IO ()
main = do
    conn <- DB.open "game-comment.sqlite3"
    S.scotty 6483 $ do
        S.get "/games/lg/:gameId" $ do
            gameIdStr <- S.param "gameId"
            case U.stringToInt gameIdStr of
                Nothing -> S.status Status.status400 >> jsonMsg "LG game id must be a number"
                Just gameId ->
                    getGame fetchLittleGolemGameRecord conn "lg" gameId

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
