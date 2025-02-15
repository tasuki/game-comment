{-# LANGUAGE OverloadedStrings #-}

import qualified Network.Wai.Handler.Warp as Warp

import qualified Api
import qualified Database as DB
import qualified Env
import qualified WebServer

main :: IO ()
main = do
    config <- Env.readEnvVars
    conn <- DB.open $ Env.sqliteFile config
    waiApp <- WebServer.waiApp config conn
    Warp.run 6483 waiApp
