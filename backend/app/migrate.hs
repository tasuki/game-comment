{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (forM_, unless)
import qualified Data.Text as T
import Database.SQLite.Simple
import System.Directory (listDirectory)
import System.FilePath ((</>), takeFileName)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Time.Clock (UTCTime)

import Database (openDb)

applyMigration :: Connection -> FilePath -> IO ()
applyMigration conn file = do
    migration <- readFile ("migrations" </> file)
    execute_ conn (Query $ T.pack migration)
    execute conn "INSERT INTO migrations (name) VALUES (?)" (Only $ takeFileName file)

wasMigrationAppliedAt :: Connection -> FilePath -> IO (Maybe String)
wasMigrationAppliedAt conn file = do
    result <- query conn "SELECT applied_at FROM migrations WHERE name = ?" (Only $ takeFileName file) :: IO [Only String]
    return $ listToMaybe [ts | Only ts <- result]

main :: IO ()
main = do
    conn <- openDb "game-comment.sqlite3"
    execute_ conn "CREATE TABLE IF NOT EXISTS migrations \
        \(name TEXT PRIMARY KEY, applied_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP)"
    files <- listDirectory "migrations"
    forM_ (sort files) $ \file -> do
        maybeAppliedAt <- wasMigrationAppliedAt conn file
        case maybeAppliedAt of
            Just appliedAt ->
                putStrLn $ file ++ " - migration already applied at " ++ appliedAt
            Nothing -> do
                putStrLn $ file ++ " - applying migration"
                applyMigration conn file
