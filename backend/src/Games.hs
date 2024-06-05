{-# LANGUAGE OverloadedStrings #-}

module Games where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text.Lazy (Text, unpack)
import qualified Database.SQLite.Simple as SQL
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as Status
import Text.Printf (printf)
import qualified Web.Scotty as S

import qualified Api
import qualified Database as DB


-- General game-fetching shenanigans

type GameFetched = Either HTTP.HttpException (Status.Status, LBS.ByteString)
type GameFetcher = Text -> IO GameFetched

maybeSaveRecord :: SQL.Connection -> Text -> Text -> GameFetched -> IO (DB.SqlResult ())
maybeSaveRecord conn source gameId eitherResult =
    case eitherResult of
        Right (responseStatus, sgf) | responseStatus == Status.status200 -> do
            _ <- Api.logMsg $ printf "Saving game: %s / %s" source gameId
            DB.saveGame conn source gameId Nothing sgf
        Right (responseStatus, _) -> do
            _ <- Api.logMsg $ printf "NOT saving game, status was: %s" (show responseStatus)
            pure $ DB.Success ()
        Left e -> do
            _ <- Api.logMsg $ printf "NOT saving game, exception: %s" (show e)
            pure $ DB.Success ()

getGame :: GameFetcher -> SQL.Connection -> Text -> Text -> S.ActionM ()
getGame fetcher conn source gameId = do
    result <- liftIO $ fetcher gameId
    _ <- liftIO $ maybeSaveRecord conn source gameId result
    record <- liftIO $ DB.fetchRecord conn source gameId
    case record of
        DB.Success record -> do
            S.status Status.status200
            S.setHeader "Content-Type" "application/sgf" -- charset is illegal anyway
            S.raw record
        _ ->
            Api.jsonError Status.status404 "Game record not found"


-- Specific providers

fetchLittleGolemGameRecord :: GameFetcher
fetchLittleGolemGameRecord gameId = do
    let gameUrl = "https://www.littlegolem.net/servlet/sgf/" <> unpack gameId <> "/game.sgf"
    try $ do
        _ <- Api.logMsg $ printf "Fetching game: %s" gameUrl
        manager <- HTTP.newManager TLS.tlsManagerSettings
        request <- HTTP.parseRequest gameUrl
        response <- HTTP.httpLbs request manager
        pure (HTTP.responseStatus response, HTTP.responseBody response)

fetchFail :: GameFetcher
fetchFail gameId = do
    pure $ Left $ HTTP.InvalidUrlException "" "This is a `here` game"
