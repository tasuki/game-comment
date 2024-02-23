{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Exception (try)
import Database.SQLite.Simple

import ApiResources
import Passwords

initialize :: String -> IO Connection
initialize dbFileName = do
    conn <- open dbFileName
    execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, username TEXT UNIQUE, password TEXT)"
    return conn

data SqlResult
    = Success
    | ConstraintError
    | OtherError

createUser :: Connection -> User -> IO SqlResult
createUser conn user = do
    result <- try (execute conn query (username user, hashPassword $ password user))
    case result of
        Left (SQLError ErrorConstraint _ _) -> return ConstraintError
        Left err -> return OtherError
        Right _ -> return Success
    where query = "INSERT INTO users (username, password) VALUES (?, ?)"