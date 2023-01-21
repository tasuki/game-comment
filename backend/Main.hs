{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (Status)
import Web.Scotty

fetchGameRecord :: String -> IO (Status, L8.ByteString)
fetchGameRecord gameId = do
    let gameUrl = "https://www.littlegolem.net/servlet/sgf/" <> gameId <> "/game.sgf"
    manager <- newManager tlsManagerSettings
    request <- parseRequest gameUrl
    response <- httpLbs request manager
    return $ (responseStatus response, responseBody response)

main :: IO ()
main = do
    scotty 6483 $ do
        get "/game/lg/:gameId" $ do
            gameId <- param "gameId"
            (responseStatus, response) <- liftIO $ fetchGameRecord gameId
            status responseStatus -- we don't mind too bad if this is off
            setHeader "Content-Type" "application/sgf; charset=iso-8859-1"
            setHeader "Access-Control-Allow-Origin" allowOrigin
            raw $ response
