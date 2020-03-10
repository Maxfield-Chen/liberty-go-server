{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module GameDB where

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Data.Text (Text)
import Game

data UserT f
  = User {_userId :: Columnar f Int
         ,_userEmail :: Columnar f Text
         ,_userName :: Columnar f Text
         ,_userPasswordHash :: Columnar f Text
         ,_userRank :: Columnar f Double} deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = UserId . _userId

data GameRecordT f
  = GameRecord {_gameId :: Columnar f Int
               ,_game :: Columnar f Game
               ,_whitePlayer :: PrimaryKey UserT f
               ,_blackPlayer :: PrimaryKey UserT f
               } deriving (Generic, Beamable)

type GameRecord = GameRecordT Identity
deriving instance Show GameRecord
deriving instance Show (PrimaryKey UserT Identity)

instance Table GameRecordT where
  data PrimaryKey GameRecordT f = GameRecordId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = GameRecordId . _gameId

type GameRecordId = PrimaryKey GameRecordT Identity

data LGSDb f =
  LGSDb
    { _LGSUsers :: f (TableEntity UserT)
     ,_LGSGameRecords :: f (TableEntity GameRecordT)
    }
  deriving (Generic, Database be)

lgsDb :: DatabaseSettings be LGSDb
lgsDb =
  defaultDbSettings
