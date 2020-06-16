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
    grBlackPlayer  :: Int,
    grWhitePlayer  :: Int,
    grBlackTeacher :: Maybe Int,
    grWhiteTeacher :: Maybe Int,
    grBlackFocus   :: Text,
    grWhiteFocus   :: Text
 } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)


newGameRecord = GameRecord (-1) G.newGame (-1) (-1) Nothing Nothing "" ""

convertGR :: GDB.GameRecord -> GameRecord
convertGR GDB.GameRecord{..} =
  let (GDB.UserId bp) = _black_player
      (GDB.UserId wp) = _white_player
      (GDB.UserId bt) = _black_teacher
      (GDB.UserId wt) = _white_teacher
  in GameRecord _gameId _game bp wp bt wt _black_focus _white_focus
