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

data UserData = UserData
    { id :: Int
    , username :: Text
    } deriving (Show, Generic)
instance A.FromJSON UserData
instance A.ToJSON UserData
--instance FromRow UserData where
--    fromRow = UserData <$> field <*> field


data CreateSession = CreateSession
    { username :: Text
    , password :: Text
    } deriving (Show, Generic)
instance A.FromJSON CreateSession
instance A.ToJSON CreateSession

data SessionData = SessionData
    { authToken :: Text
    } deriving (Show, Generic)
instance A.FromJSON SessionData
instance A.ToJSON SessionData
