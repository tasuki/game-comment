{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ApiResources where

import qualified Data.Aeson as A
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)


data CreateUser = CreateUser
    { username :: Text
    , password :: Text
    , favorite :: Text
    , email :: Maybe Text
    } deriving (Show, Generic)
instance A.FromJSON CreateUser

data UpdatePassword = UpdatePassword
    { password :: Text
    } deriving (Show, Generic)
instance A.FromJSON UpdatePassword

data UserData = UserData
    { id :: Int
    , username :: Text
    } deriving (Show, Generic)
instance A.ToJSON UserData


data CreateSession = CreateSession
    { username :: Text
    , password :: Text
    } deriving (Show, Generic)
instance A.FromJSON CreateSession

data SessionData = SessionData
    { authToken :: Text
    } deriving (Show, Generic)
instance A.ToJSON SessionData


data GetGame = GetGame
    { source :: Text
    , gameId :: Text
    , userId :: Int
    } deriving (Show, Generic)
instance A.ToJSON GetGame


data CreateComment = CreateComment
    { comment :: Text
    } deriving (Show, Generic)
instance A.FromJSON CreateComment

data Comment = Comment
    { commentId :: Int
    , userId :: Int
    , username :: Text
    , comment :: Text
    , created :: Text
    } deriving (Show, Generic)
instance A.ToJSON Comment
instance A.FromJSON Comment
