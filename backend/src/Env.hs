{-# LANGUAGE OverloadedStrings #-}

module Env where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv)
import System.Exit (die)

data Config = Config
    { allowOrigin :: String
    , jwtSecret :: String
    , passSalt :: String
    }

readOne :: String -> IO String
readOne key = do
    maybeVal <- lookupEnv key
    case maybeVal of
        Just v -> pure v
        Nothing -> die $ key ++ " not found in .env file"

readEnvVars :: IO Config
readEnvVars = do
    loadFile defaultConfig

    allowOrigin <- readOne "ALLOW_ORIGIN"
    jwtSecret <- readOne "JWT_SECRET"
    passSalt <- readOne "PASS_SALT"

    pure $ Config allowOrigin jwtSecret passSalt
