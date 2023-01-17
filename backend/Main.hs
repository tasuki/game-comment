{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Scotty

fetchGameRecord :: String -> IO L8.ByteString
fetchGameRecord gameId = do
    let gameUrl = "https://www.littlegolem.net/servlet/sgf/" <> gameId <> "/game.sgf"
    manager <- newManager tlsManagerSettings
    request <- parseRequest gameUrl
    response <- httpLbs request manager
    return $ responseBody response

main :: IO ()
main = do
    scotty 6483 $ do
        get "/game/:gameId" $ do
            gameId <- param "gameId"
            response <- liftIO $ fetchGameRecord gameId
            setHeader "Content-Type" "application/sgf; charset=iso-8859-1"
            raw $ response
