{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (forM_, unless)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as S
import System.Directory (listDirectory)
import System.FilePath ((</>), takeFileName)
import Data.List (sort)
import Data.Maybe (listToMaybe)

import qualified Database as DB

applyMigration :: S.Connection -> FilePath -> IO ()
applyMigration conn file = do
    migration <- readFile ("migrations" </> file)
    S.execute_ conn (S.Query $ T.pack migration)
    S.execute conn "INSERT INTO migrations (name) VALUES (?)" (S.Only $ takeFileName file)

wasMigrationAppliedAt :: S.Connection -> FilePath -> IO (Maybe String)
wasMigrationAppliedAt conn file = do
    result <- S.query conn "SELECT applied_at FROM migrations WHERE name = ?" (S.Only $ takeFileName file) :: IO [S.Only String]
    return $ listToMaybe [ts | S.Only ts <- result]

main :: IO ()
main = do
    conn <- DB.open "game-comment.sqlite3"
    S.execute_ conn "CREATE TABLE IF NOT EXISTS migrations \
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
