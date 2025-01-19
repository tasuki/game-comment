{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Monad (when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson ((.=))
import Data.Text.Lazy (Text, unpack)
import qualified Data.Time.Clock as TC
import qualified Data.Time.Clock.System as TCS
import qualified Data.Time.Format as TF
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types.Status as Status
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import qualified Web.Scotty as S

import Auth
import qualified ApiResources as Res

logMsg :: String -> IO ()
logMsg str = do
    currentTime <- TC.getCurrentTime
    let timestamp = TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    putStrLn $ timestamp ++ " " ++ str

jsonMsg :: Text -> S.ActionM ()
jsonMsg msg = S.json $ A.object [ "msg" .= (msg :: Text) ]

customErrorHandler :: Text -> S.ActionM ()
customErrorHandler msg = jsonMsg msg

jsonError :: Status.Status -> String -> S.ActionM a
jsonError st msg = do
    S.status st
    S.json $ A.object [ "msg" .= (TL.fromStrict $ T.pack msg) ]
    S.finish

jsonData :: (A.FromJSON a) => S.ActionM a
jsonData = do
    b <- S.body
    when (b == "") $ do
        jsonError Status.status400 "jsonData - No data was provided."
    case A.eitherDecode b of
        Left err -> jsonError Status.status400 $
            "jsonData - malformed: " `mappend` LBS.unpack b
                `mappend` " Error was: " `mappend` err
        Right value -> case A.fromJSON value of
            A.Error err -> do
                jsonError Status.status400 $
                    "jsonData - failed parsing: " `mappend` LBS.unpack b
                        `mappend` " Error was: " `mappend` err
            A.Success a -> do
                pure a

addDefaultHeaders :: String -> Middleware
addDefaultHeaders allowOrigin = addHeaders
    [ ("Access-Control-Allow-Origin", BS.pack allowOrigin ) ]

onlySignedIn :: String -> (Res.UserData -> S.ActionM ()) -> S.ActionM ()
onlySignedIn secretKey action = do
    let respondUnauthorized = jsonError Status.status401 "You must be logged in"
    mToken <- S.header "Authorization"
    case mToken of
        Nothing -> do
            respondUnauthorized
        Just token -> do
            -- Remove "Bearer " prefix
            let jwtToken = drop 7 $ unpack token
            case Auth.verifyJwt secretKey jwtToken of
                Nothing -> respondUnauthorized
                Just userData -> action userData

getCurrentUnixTime :: IO Integer
getCurrentUnixTime = do
    currentTime <- TCS.getSystemTime
    let (TCS.MkSystemTime seconds _) = currentTime
    pure $ fromIntegral seconds
