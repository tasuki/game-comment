{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Exception (try)
import qualified Database.SQLite.Simple as S
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified ApiResources as AR
import qualified ApiResources as CU (CreateUser(username, password, email))
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
    return $ case result of
        Right _ -> Success ()
        Left (S.SQLError S.ErrorConstraint _ _) -> ConstraintError
        Left _ -> OtherError

createUser :: S.Connection -> String -> CU.CreateUser -> IO (SqlResult ())
createUser conn salt createUser = do
    result <- try $ S.execute conn query
        ( CU.username createUser
        , hashPassword salt $ CU.password createUser
        , CU.email createUser
        )
    writeResult result
    where query = "INSERT INTO users (username, password, email) VALUES (?, ?, ?)"

authenticateUser :: S.Connection -> CS.CreateSession -> IO (SqlResult (Either String AR.UserData))
authenticateUser conn createSession = do
    rows <- S.query conn query [CS.username createSession] :: IO [(Int, Text, Text)]
    return $ case rows of
        [(id, username, pass)] -> Success $
            if verifyPassword pass $ CS.password createSession then
                Right $ AR.UserData id username
            else
                Left "Username and password don't match"
        [] -> Success $ Left "No such user exists"
        _ -> OtherError
    where query = "SELECT id, username, password FROM users WHERE username = ?"

saveRecord :: S.Connection -> Text -> Text -> LBS.ByteString -> IO (SqlResult ())
saveRecord conn source id sgf = do
    result <- try $ S.execute conn query (source, id, sgf)
    writeResult result
    where query = "REPLACE INTO games (source, id, sgf) VALUES (?, ?, ?)"

fetchRecord :: S.Connection -> Text -> Text -> IO (SqlResult LBS.ByteString)
fetchRecord conn source id = do
    rows <- S.query conn query (source, id) :: IO [S.Only LBS.ByteString]
    return $ case rows of
        [S.Only sgf] -> Success $ sgf
        _ -> OtherError
    where query = "SELECT sgf FROM games WHERE source = ? AND id = ?"
