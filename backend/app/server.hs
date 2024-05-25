{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text.Lazy (Text, unpack)
import qualified Data.Time.Clock as TC
import qualified Data.Time.Clock.System as TCS
import qualified Data.Time.Format as TF
import qualified Database.SQLite.Simple as SQL
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as Status
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Text.Printf (printf)
import qualified Web.Scotty as S

import qualified ApiResources as API
import qualified Auth as A
import qualified Database as DB
import qualified Env as E
import qualified Utils as U


logMsg :: String -> IO ()
logMsg str = do
    currentTime <- TC.getCurrentTime
    let timestamp = TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    putStrLn $ timestamp ++ " " ++ str

jsonMsg :: Text -> S.ActionM ()
jsonMsg msg = S.json $ object [ "msg" .= (msg :: Text) ]

jsonMsgStr :: String -> S.ActionM ()
jsonMsgStr = stringToLazyText >> jsonMsg

addDefaultHeaders :: String -> Middleware
addDefaultHeaders allowOrigin = addHeaders
    [ ("Access-Control-Allow-Origin", U.stringToByteString allowOrigin ) ]

type GameFetched = Either HTTP.HttpException (Status.Status, LBS.ByteString)

fetchLittleGolemGameRecord :: Text -> IO GameFetched
fetchLittleGolemGameRecord gameId = do
    let gameUrl = "https://www.littlegolem.net/servlet/sgf/" <> (unpack gameId) <> "/game.sgf"
    try $ do
        _ <- logMsg $ printf "Fetching game: %s" gameUrl
        manager <- HTTP.newManager TLS.tlsManagerSettings
        request <- HTTP.parseRequest gameUrl
        response <- HTTP.httpLbs request manager
        return (HTTP.responseStatus response, HTTP.responseBody response)

maybeSaveRecord :: SQL.Connection -> Text -> Text -> GameFetched -> IO (DB.SqlResult ())
maybeSaveRecord conn source gameId eitherResult =
    case eitherResult of
        Right (responseStatus, sgf) | responseStatus == Status.status200 -> do
            _ <- logMsg $ printf "Saving game: %s / %s" source gameId
            DB.saveRecord conn source gameId sgf
        Right (responseStatus, _) -> do
            _ <- logMsg $ printf "NOT saving game, status was: %s" (show responseStatus)
            return $ DB.Success ()
        Left e -> do
            _ <- logMsg $ printf "NOT saving game, exception: %s" (show e)
            return $ DB.Success ()

getGame :: E.Config -> (Text -> IO GameFetched) -> SQL.Connection -> Text -> Text -> S.ActionM ()
getGame config fetcher conn source gameId = do
    result <- liftIO $ fetcher gameId
    _ <- liftIO $ maybeSaveRecord conn source gameId result
    record <- liftIO $ DB.fetchRecord conn source gameId
    case record of
        DB.OtherError ->
            S.status Status.status404 >> jsonMsg "Game record not found"
        DB.Success record -> do
            S.status Status.status200
            S.setHeader "Content-Type" "application/sgf" -- charset is illegal anyway
            S.raw record

getCurrentUnixTime :: IO Integer
getCurrentUnixTime = do
    currentTime <- TCS.getSystemTime
    let (TCS.MkSystemTime seconds _) = currentTime
    return $ fromIntegral seconds

main :: IO ()
main = do
    config <- E.readEnvVars
    conn <- DB.open "game-comment.sqlite3"
    S.scotty 6483 $ do
        S.middleware $ addDefaultHeaders $ E.allowOrigin config
        S.get "/games/lg/:gameId" $ do
            gameId <- S.param "gameId"
            case U.stringToInt gameId of
                Nothing ->
                    S.status Status.status400 >> jsonMsg "LG game id must be a number"
                Just _ ->
                    getGame config fetchLittleGolemGameRecord conn "lg" $ U.stringToLazyText gameId

        S.post "/users" $ do
            user <- S.jsonData :: S.ActionM API.CreateUser
            creationResult <- liftIO $ DB.createUser conn (E.passSalt config) user
            case creationResult of
                DB.Success () -> jsonMsg "User created successfully"
                DB.ConstraintError -> S.status Status.status409 >> jsonMsg "Username already exists"
                _ -> S.status Status.status500 >> jsonMsg "Unknown error"

        S.post "/sessions" $ do
            nowTime <- liftIO getCurrentUnixTime
            user <- S.jsonData :: S.ActionM API.CreateSession
            creationResult <- liftIO $ DB.authenticateUser conn user
            case creationResult of
                DB.Success (Right userData) ->
                    S.json $ object [ "authToken" .= (A.createJwt (E.jwtSecret config) nowTime userData) ]
                DB.Success (Left msg) ->
                    S.status Status.status401 >> jsonMsgStr msg
                _ ->
                    S.status Status.status500 >> jsonMsg "Unknown error"
