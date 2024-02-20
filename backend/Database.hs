{-# LANGUAGE OverloadedStrings #-}

module Database (User, createUser, initialize) where

import Database.SQLite.Simple

import ApiResources
import Passwords

initialize :: String -> IO Connection
initialize dbFileName = do
    conn <- open dbFileName
    execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, username TEXT UNIQUE, password TEXT)"
    return conn

createUser :: Connection -> User -> IO ()
createUser conn user =
    execute conn "INSERT INTO users (username, password) VALUES (?, ?)" (username user, hashPassword $ password user)
