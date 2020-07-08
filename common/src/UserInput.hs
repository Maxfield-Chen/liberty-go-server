{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module UserInput where

import           Data.Aeson.Types
import           Data.Text           (Text)
import           GHC.Generics
import           Servant.Auth.Server

data RegisterUser =
  RegisterUser
  {
    rUserEmail    :: Text,
    rUserName     :: Text,
    rUserImage     :: Int,
    rUserPassword :: Text
 } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)


data User =
  User
    { userEmail :: Text
    , userName  :: Text
    , userImage  :: Int
    , userId    :: Int
    } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)

data  ChatMessage =
   ChatMessage { chatMessage     :: Text,
                 chatShared ::  Bool} deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)
data Login =
  Login { loginName     :: Text,
          loginPassword :: Text} deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data ProposedGame =
  ProposedGame {_pg_black_player :: Int,
           _pg_white_player      :: Int,
           _pg_black_teacher     :: Maybe Int,
           _pg_white_teacher     :: Maybe Int,
           _pg_black_focus       :: Text,
           _pg_white_focus       :: Text
           } deriving (Generic, ToJSON, FromJSON)
