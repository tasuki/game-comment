{-# LANGUAGE OverloadedStrings #-}

module Config (allowOrigin) where

import qualified Data.Text.Lazy as TL

allowOrigin :: TL.Text
allowOrigin = "games.tasuki.org"
