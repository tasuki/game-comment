{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty

main = do
    putStrLn "Starting Server..."
    scotty 6483 $ do
        get "/game/:gameId" $ do
            gameId <- param "gameId"
            text $ "hello " <> gameId <> " world"
