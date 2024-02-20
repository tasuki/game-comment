{-# LANGUAGE DeriveGeneric #-}

module ApiResources where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)

data User = User
    { username :: Text
    , password :: Text
    } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User
