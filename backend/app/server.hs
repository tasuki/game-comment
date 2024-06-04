{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types.Status as Status
import qualified Web.Scotty as S

import Api (jsonData, jsonError, jsonMsg, onlySignedIn)
import qualified Api
import qualified ApiResources as Res
import qualified Auth
import qualified Database as DB
import qualified Env
import qualified Games
import qualified Utils as U

main :: IO ()
main = do
    config <- Env.readEnvVars
    conn <- DB.open "game-comment.sqlite3"
    S.scotty 6483 $ do
        S.middleware $ Api.addDefaultHeaders $ Env.allowOrigin config
        S.defaultHandler Api.customErrorHandler

        S.get "/games/lg/:gameId" $ do
            gameId <- S.param "gameId"
            case U.stringToInt gameId of
                Nothing ->
                    jsonError Status.status400 "LG game id must be a number"
                Just _ ->
                    Games.getGame Games.fetchLittleGolemGameRecord conn "lg" $ U.stringToLazyText gameId

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
                    jsonError Status.status401 $ U.stringToLazyText msg
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
