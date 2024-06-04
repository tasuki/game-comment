{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Exception (try)
import qualified Database.SQLite.Simple as S
import Data.Text.Lazy (Text)
import Data.Tuple.Curry (uncurryN)
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified ApiResources as API
import qualified ApiResources as CU (CreateUser(username, password, email))
import qualified ApiResources as UP (UpdatePassword(password))
import qualified ApiResources as UD (UserData(id, username))
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

authenticateUser :: S.Connection -> CS.CreateSession -> IO (SqlResult (Either String API.UserData))
authenticateUser conn createSession = do
    rows <- S.query conn query [CS.username createSession] :: IO [(Int, Text, Text)]
    return $ case rows of
        [(id, username, pass)] -> Success $
            if verifyPassword pass $ CS.password createSession then
                Right $ API.UserData id username
            else
                Left "Username and password don't match"
        [] -> Success $ Left "No such user exists"
        _ -> OtherError
    where query = "SELECT id, username, password FROM users WHERE username = ?"

updatePassword :: S.Connection -> String -> API.UserData -> UP.UpdatePassword -> IO (SqlResult ())
updatePassword conn salt userData updatePassword = do
    result <- try $ S.execute conn query
        ( hashPassword salt $ UP.password updatePassword
        , UD.id userData
        )
    writeResult result
    where query = "UPDATE users SET password = ? WHERE id = ?"

saveRecord :: S.Connection -> Text -> Text -> LBS.ByteString -> IO (SqlResult ())
saveRecord conn source gameId sgf = do
    result <- try $ S.execute conn query (source, gameId, sgf)
    writeResult result
    where query = "REPLACE INTO games (source, game_id, sgf) VALUES (?, ?, ?)"

fetchRecord :: S.Connection -> Text -> Text -> IO (SqlResult LBS.ByteString)
fetchRecord conn source gameId = do
    rows <- S.query conn query (source, gameId) :: IO [S.Only LBS.ByteString]
    return $ case rows of
        [S.Only sgf] -> Success $ sgf
        _ -> OtherError
    where query = "SELECT sgf FROM games WHERE source = ? AND game_id = ?"

getComments :: S.Connection -> Text -> Text -> IO (SqlResult [API.Comment])
getComments conn source gameId = do
    rows <- S.query conn query (source, gameId) :: IO [(Int, Int, Text, Text, Text)]
    return $ Success $ map (uncurryN API.Comment) rows
    where query = "\
        \ SELECT c.id, c.user_id, u.username, c.comment, c.created \
        \ FROM comments AS c \
        \ JOIN users AS u ON u.id = c.user_id \
        \ WHERE c.source = ? AND c.game_id = ? \
        \ ORDER BY c.created"

postComment :: S.Connection -> API.UserData -> Text -> Text -> Text -> IO (SqlResult ())
postComment conn userData source gameId comment = do
    result <- try $ S.execute conn query (UD.id userData, source, gameId, comment)
    writeResult result
    where query = "INSERT INTO comments (user_id, source, game_id, comment) VALUES (?, ?, ?, ?)"
