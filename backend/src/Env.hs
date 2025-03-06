{-# LANGUAGE OverloadedStrings #-}

module Env where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)

data Config = Config
    { listenPort :: Int
    , allowOrigin :: String
    , sqliteFile :: String
    , jwtSecret :: String
    , passSalt :: String
    }

readOne :: String -> IO String
readOne key = do
    maybeVal <- lookupEnv key
    case maybeVal of
        Just v -> pure v
        Nothing -> die $ key ++ " not found in .env file"

readInt :: String -> IO Int
readInt key = do
    value <- readOne key
    maybe (die $ "Invalid integer for " ++ key ++ ": " ++ value) pure (readMaybe value)

readEnvVars :: IO Config
readEnvVars = do
    loadFile defaultConfig

    listenPort <- readInt "LISTEN_PORT"
    allowOrigin <- readOne "ALLOW_ORIGIN"
    sqliteFile <- readOne "SQLITE_FILE"
    jwtSecret <- readOne "JWT_SECRET"
    passSalt <- readOne "PASS_SALT"

    pure $ Config listenPort allowOrigin sqliteFile jwtSecret passSalt
