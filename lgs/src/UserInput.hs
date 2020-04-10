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

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                  (Text)
import           Game
import           GameDB                     hiding (User)
import           GameExpressions
import qualified GameLogic                  as GL
import           GHC.Generics
import           Prelude                    ()
import           Prelude.Compat
import           Proofs
import           Servant
import           Servant.Auth.Server
import           Theory.Named


data User =
  User
    { userEmail    :: Text
    , userName     :: Text
    , userPassword :: Text
    } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)

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
