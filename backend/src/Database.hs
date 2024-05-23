{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Exception (try)
import Database.SQLite.Simple

import qualified ApiResources as CU (CreateUser(username, password))
import qualified ApiResources as CS (CreateSession(username, password))
import Passwords
import Data.Text.Lazy (Text)

openDb :: String -> IO Connection
openDb dbFileName = do
    conn <- open dbFileName
    execute_ conn "PRAGMA foreign_keys = ON;"
    return conn

data SqlResult a
    = Success a
    | ConstraintError
    | OtherError

writeResult :: Either SQLError () -> IO (SqlResult ())
writeResult result =
    case result of
        Right _ -> return $ Success ()
        Left (SQLError ErrorConstraint _ _) -> return ConstraintError
        Left _ -> return OtherError

createUser :: Connection -> CU.CreateUser -> IO (SqlResult ())
createUser conn createUser = do
    result <- try $ execute conn query (CU.username createUser, hashPassword $ CU.password createUser)
    writeResult result
    where query = "INSERT INTO users (username, password) VALUES (?, ?)"

authenticateUser :: Connection -> CS.CreateSession -> IO (SqlResult Bool)
authenticateUser conn createSession = do
    rows <- query conn sql [CS.username createSession] :: IO [Only Text]
    case rows of
        [Only pass] -> return $ Success $ verifyPassword pass $ CS.password createSession
        _ -> return OtherError
    where
        sql = "SELECT password FROM users WHERE username = ?"

saveRecord :: Connection -> Text -> Int -> Text -> IO (SqlResult ())
saveRecord conn source id sgf = do
    result <- try $ execute conn query (source, id, sgf)
    writeResult result
    where query = "REPLACE INTO games (source, id, sgf) VALUES (?, ?, ?)"
