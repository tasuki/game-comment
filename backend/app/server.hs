{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types.Status as Status
import qualified Web.Scotty as S

import Api (jsonData, jsonError, jsonMsg, onlySignedIn)
import qualified Api
import qualified ApiResources as Res
import qualified ApiResources as UD (UserData(id))
import qualified ApiResources as GG (GetGame(userId))
import qualified Auth
import qualified Database as DB
import qualified Env
import qualified Games

main :: IO ()
main = do
    config <- Env.readEnvVars
    conn <- DB.open "game-comment.sqlite3"
    S.scotty 6483 $ do
        S.middleware $ Api.addDefaultHeaders $ Env.allowOrigin config
        S.defaultHandler Api.customErrorHandler


        -- Users
        S.post "/users" $ do
            user <- jsonData :: S.ActionM Res.CreateUser
            creationResult <- liftIO $ DB.createUser conn (Env.passSalt config) user
            case creationResult of
                DB.Success () -> jsonMsg "User created successfully"
                DB.ConstraintError -> jsonError Status.status409 "Username already exists"
                _ -> jsonError Status.status500 "Unknown error"

        S.post "/sessions" $ do
            nowTime <- liftIO Api.getCurrentUnixTime
            user <- jsonData :: S.ActionM Res.CreateSession
            creationResult <- liftIO $ DB.authenticateUser conn user
            case creationResult of
                DB.Success (Right userData) ->
                    S.json $ Res.SessionData $ Auth.createJwt (Env.jwtSecret config) nowTime userData
                DB.Success (Left msg) ->
                    jsonError Status.status401 msg
                _ ->
                    jsonError Status.status500 "Unknown error"

        S.post "/user/password" $ do
            updatePassword <- jsonData :: S.ActionM Res.UpdatePassword
            onlySignedIn (Env.jwtSecret config) (\user -> do
                changedResult <- liftIO $ DB.updatePassword conn (Env.passSalt config) user updatePassword
                case changedResult of
                    DB.Success () -> jsonMsg "Password updated"
                    _ -> jsonError Status.status500 "Unknown error"
                )


        -- Games
        S.get "/games/lg/:gameId" $ do
            gameId <- S.param "gameId"
            case reads gameId :: [(Int, String)] of
                [(x, "")] ->
                    Games.getGame Games.fetchLittleGolemGameRecord conn "lg" gameId
                _ ->
                    jsonError Status.status400 "LG game id must be a number"

        S.get "/games/here/:gameId" $ do
            let source = "here"
            gameId <- S.param "gameId"
            Games.getGame Games.fetchFail conn source gameId

        S.put "/games/here/:gameId" $ do
            let source = "here"
            gameId <- S.param "gameId"
            record <- S.body
            onlySignedIn (Env.jwtSecret config) (\user -> do
                _ <- liftIO $ DB.beginTransaction conn
                gotGame <- liftIO $ DB.getGame conn source gameId
                let saveGame = liftIO $ DB.saveGame conn source gameId (Just user) record
                savedGame <- case gotGame of
                    DB.OtherError -> saveGame -- game not exist
                    DB.Success gotGame | (GG.userId gotGame) == (UD.id user) -> saveGame -- our game
                    _ -> liftIO $ pure DB.ConstraintError -- someone else's game
                _ <- liftIO $ DB.commitTransaction conn
                case savedGame of
                    DB.Success () -> jsonMsg "Game saved"
                    DB.ConstraintError -> jsonError Status.status403 "Not your game!"
                    _ -> jsonError Status.status500 "Unknown error"
                )

        -- Comments
        S.get "/games/:source/:gameId/comments" $ do
            gameId <- S.param "gameId"
            source <- S.param "source"
            commentResult <- liftIO $ DB.getComments conn source gameId
            case commentResult of
                DB.Success comments -> S.json comments
                _ -> jsonError Status.status500 "Unknown error"

        S.post "/games/:source/:gameId/comments" $ do
            onlySignedIn (Env.jwtSecret config) (\user -> do
                gameId <- S.param "gameId"
                source <- S.param "source"
                Res.CreateComment comment <- jsonData :: S.ActionM Res.CreateComment
                commentResult <- liftIO $ DB.postComment conn user source gameId comment
                case commentResult of
                    DB.Success () -> jsonMsg "Comment posted"
                    DB.ConstraintError -> jsonError Status.status409 "No such game, or dupe comment, your choice!"
                    _ -> jsonError Status.status500 "Unknown error"
                )

        S.notFound $
            jsonError Status.status404 "Resource not found - check the method and the path"
