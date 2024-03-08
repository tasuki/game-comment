{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ApiResources where

import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)


data CreateUser = CreateUser
    { username :: Text
    , password :: Text
    } deriving (Show, Generic)

instance FromJSON CreateUser
instance ToJSON CreateUser


data CreateSession = CreateSession
    { username :: Text
    , password :: Text
    } deriving (Show, Generic)

instance FromJSON CreateSession
instance ToJSON CreateSession