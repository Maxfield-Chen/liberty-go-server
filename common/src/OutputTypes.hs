{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module OutputTypes where

import           Data.Aeson.Types
import qualified Data.HashMap.Strict as M
import           Data.Text           (Text)
import qualified Game                as G
import qualified GameDB              as GDB
import           GHC.Generics
import           Servant.Auth.Server

data GameRecord =
  GameRecord
  {
    grId           :: Int,
    grGame         :: G.Game,
    grBlackPlayer  :: User,
    grWhitePlayer  :: User,
    grBlackTeacher :: Maybe User,
    grWhiteTeacher :: Maybe User,
    grBlackFocus   :: Text,
    grWhiteFocus   :: Text
 } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)


newGameRecord = GameRecord (-1) G.newGame newUser newUser Nothing Nothing "" ""

convertGR :: GDB.GameRecord ->
             GDB.User ->
             GDB.User ->
             Maybe GDB.User ->
             Maybe GDB.User ->
             GameRecord
convertGR GDB.GameRecord{..} bp wp mbt mwt =
  GameRecord
    _gameId
    _game
    (convertUser bp)
    (convertUser wp)
    (convertUser <$> mbt)
    (convertUser <$> mwt)
    _black_focus
    _white_focus

data Awaiter =
  Awaiter
  {
    awaiterUser :: Int,
    awaiterGame :: Int
  } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)

convertAwaiter :: GDB.Awaiter -> Awaiter
convertAwaiter GDB.Awaiter{..} =
  let (GDB.UserId p) = _awaiter_user_id
      (GDB.GameRecordId g) = _awaiter_game_id
  in Awaiter p g

data User =
  User
  {
    userId    :: Int,
    userName  :: Text,
    userEmail :: Text
  } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)

newUser = User (-1) "" ""
convertUser :: GDB.User -> User
convertUser GDB.User{..} = User _userId _userName _userEmail

type AllGames = ([GameRecord], M.HashMap Int [Awaiter])
