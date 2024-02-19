{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Database (User, createUser, initialize) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy (Text)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Passwords

data User = User
    { username :: Text
    , password :: Text
    } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

initialize :: String -> IO Connection
initialize dbFileName = do
    conn <- open dbFileName
    execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, password TEXT)"
    return conn

createUser :: Connection -> User -> IO ()
createUser conn user =
    execute conn "INSERT INTO users (username, password) VALUES (?, ?)" (username user, hashPassword $ password user)
