{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Lazy (Text)
import Network.HTTP.Client (httpLbs, newManager, parseRequest, Response(responseStatus, responseBody))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Web.Scotty

import ApiResources
import Database

fetchGameRecord :: String -> IO (Status, L8.ByteString)
fetchGameRecord gameId = do
    let gameUrl = "https://www.littlegolem.net/servlet/sgf/" <> gameId <> "/game.sgf"
    manager <- newManager tlsManagerSettings
    request <- parseRequest gameUrl
    response <- httpLbs request manager
    return (responseStatus response, responseBody response)

main :: IO ()
main = do
    conn <- initialize "game-comment.sqlite3"
    scotty 6483 $ do
        get "/game/lg/:gameId" $ do
            gameId <- param "gameId"
            (responseStatus, response) <- liftIO $ fetchGameRecord gameId
            status responseStatus -- we don't mind too bad if this is off
            setHeader "Content-Type" "application/sgf; charset=iso-8859-1"
            setHeader "Access-Control-Allow-Origin" allowOrigin
            raw response

        post "/users" $ do
            user <- jsonData :: ActionM User
            creationResult <- liftIO $ createUser conn user
            case creationResult of
                Success -> json $ object [ "msg" .= ("User created successfully" :: Text) ]
                ConstraintError ->
                    status status409 >> json (object [ "msg" .= ("Username already exists" :: Text) ])
                _ -> status status500 >> json (object [ "msg" .= ("Unknown error" :: Text) ])
