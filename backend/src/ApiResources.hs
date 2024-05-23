{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ApiResources where

import qualified Data.Aeson as A
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)


data CreateUser = CreateUser
    { username :: Text
    , password :: Text
    , email :: Maybe Text
    } deriving (Show, Generic)

instance A.FromJSON CreateUser
instance A.ToJSON CreateUser


data CreateSession = CreateSession
    { username :: Text
    , password :: Text
    } deriving (Show, Generic)

instance A.FromJSON CreateSession
instance A.ToJSON CreateSession
