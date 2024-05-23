{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Exception (try)
import qualified Database.SQLite.Simple as S
import Data.Text.Lazy (Text)

import qualified ApiResources as CU (CreateUser(username, password))
import qualified ApiResources as CS (CreateSession(username, password))
import Passwords (hashPassword, verifyPassword)

open :: String -> IO S.Connection
open dbFileName = do
    conn <- S.open dbFileName
    S.execute_ conn "PRAGMA foreign_keys = ON;"
    return conn

data SqlResult a
    = Success a
    | ConstraintError
    | OtherError

writeResult :: Either S.SQLError () -> IO (SqlResult ())
writeResult result =
    case result of
        Right _ -> return $ Success ()
        Left (S.SQLError S.ErrorConstraint _ _) -> return ConstraintError
        Left _ -> return OtherError

createUser :: S.Connection -> CU.CreateUser -> IO (SqlResult ())
createUser conn createUser = do
    result <- try $ S.execute conn query (CU.username createUser, hashPassword $ CU.password createUser)
    writeResult result
    where query = "INSERT INTO users (username, password) VALUES (?, ?)"

authenticateUser :: S.Connection -> CS.CreateSession -> IO (SqlResult Bool)
authenticateUser conn createSession = do
    rows <- S.query conn sql [CS.username createSession] :: IO [S.Only Text]
    case rows of
        [S.Only pass] -> return $ Success $ verifyPassword pass $ CS.password createSession
        _ -> return OtherError
    where
        sql = "SELECT password FROM users WHERE username = ?"

saveRecord :: S.Connection -> Text -> Int -> Text -> IO (SqlResult ())
saveRecord conn source id sgf = do
    result <- try $ S.execute conn query (source, id, sgf)
    writeResult result
    where query = "REPLACE INTO games (source, id, sgf) VALUES (?, ?, ?)"
