{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Exception (try)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import qualified ApiResources as CU (CreateUser(username, password))
import qualified ApiResources as CS (CreateSession(username, password))
import Passwords
import Data.Text.Lazy (Text)

initialize :: String -> IO Connection
initialize dbFileName = do
    conn <- open dbFileName
    execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, username TEXT UNIQUE, password TEXT)"
    return conn

data SqlResult a
    = Success a
    | ConstraintError
    | OtherError

createUser :: Connection -> CU.CreateUser -> IO (SqlResult ())
createUser conn createUser = do
    result <- try (execute conn query (CU.username createUser, hashPassword $ CU.password createUser))
    case result of
        Left (SQLError ErrorConstraint _ _) -> return ConstraintError
        Left _ -> return OtherError
        Right _ -> return $ Success ()
    where query = "INSERT INTO users (username, password) VALUES (?, ?)"

authenticateUser :: Connection -> CS.CreateSession -> IO (SqlResult Bool)
authenticateUser conn createSession = do
    rows <- query conn sql [CS.username createSession] :: IO [Only Text]
    case rows of
        [Only pass] -> return $ Success (verifyPassword pass $ CS.password createSession)
        _ -> return OtherError
    where
        sql = "SELECT password FROM users WHERE username = ?"