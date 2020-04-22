{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-- Needed for Beam Lenses
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module GameDB where

import           Data.Aeson.Types
import           Data.Text                 (Text, unpack)
import qualified Data.Time                 as Time
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite
import           Game
import           Servant.Auth.Server

data UserT f
  = User {_userId           :: Columnar f Int
         ,_userEmail        :: Columnar f Text
         ,_userName         :: Columnar f Text
         ,_userPasswordHash :: Columnar f Text} deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User
deriving instance ToJWT User
deriving instance FromJWT User
deriving instance ToJSON User
deriving instance FromJSON User
deriving instance Eq (PrimaryKey UserT Identity)
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show (PrimaryKey UserT (Nullable Identity))
deriving instance ToJSON (PrimaryKey UserT Identity)
deriving instance ToJSON (PrimaryKey UserT (Nullable Identity))
deriving instance FromJSON (PrimaryKey UserT Identity)
deriving instance FromJSON (PrimaryKey UserT (Nullable Identity))

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = UserId . _userId

User (LensFor userId) (LensFor userEmail)
     (LensFor userName) (LensFor userPasswordHash)
     = tableLenses


data AwaiterT f
  = Awaiter {_awaiter_id      :: Columnar f Int
            ,_awaiter_user_id :: PrimaryKey UserT f
            ,_awaiter_game_id :: PrimaryKey GameRecordT f} deriving (Generic, Beamable)

type Awaiter = AwaiterT Identity
type AwaiterId = PrimaryKey AwaiterT Identity

deriving instance Eq Awaiter
deriving instance Show Awaiter
deriving instance ToJSON Awaiter
deriving instance FromJSON Awaiter

deriving instance Show (PrimaryKey AwaiterT Identity)
deriving instance ToJSON (PrimaryKey AwaiterT Identity)
deriving instance FromJSON (PrimaryKey AwaiterT Identity)

instance Table AwaiterT where
  data PrimaryKey AwaiterT f = AwaiterId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = AwaiterId . _awaiter_id


Awaiter (LensFor awaiter_id) (UserId (LensFor awaiter_user_id))
        (GameRecordId (LensFor awaiter_game_id))
        = tableLenses

data GameRecordT f
  = GameRecord {_gameId        :: Columnar f Int
               ,_game          :: Columnar f Game
               ,_black_player  :: PrimaryKey UserT f
               ,_white_player  :: PrimaryKey UserT f
               ,_black_teacher :: PrimaryKey UserT (Nullable f)
               ,_white_teacher :: PrimaryKey UserT (Nullable f)
               ,_black_focus   :: Columnar f Text
               ,_white_focus   :: Columnar f Text
               ,_timestamp     :: Columnar f Time.LocalTime
               } deriving (Generic, Beamable)

type GameRecord = GameRecordT Identity
deriving instance Show GameRecord
deriving instance ToJSON GameRecord
deriving instance FromJSON GameRecord
deriving instance Eq (PrimaryKey GameRecordT Identity)
deriving instance Show (PrimaryKey GameRecordT Identity)
deriving instance ToJSON (PrimaryKey GameRecordT Identity)
deriving instance FromJSON (PrimaryKey GameRecordT Identity)

instance Table GameRecordT where
  data PrimaryKey GameRecordT f = GameRecordId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = GameRecordId . _gameId

type GameRecordId = PrimaryKey GameRecordT Identity

GameRecord (LensFor grId) (LensFor game)
           (UserId (LensFor blackPlayer)) (UserId (LensFor whitePlayer))
           (UserId (LensFor blackTeacher)) (UserId (LensFor whiteTeacher))
           (LensFor blackFocus) (LensFor whiteFocus)
           (LensFor timestamp)
           = tableLenses

-- LGL SQL Representation Definitions
instance HasSqlValueSyntax be String => HasSqlValueSyntax be Game where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite Game where
  fromBackendRow = read . unpack <$> fromBackendRow


data LGSDb f =
  LGSDb
    { users        :: f (TableEntity UserT)
     ,game_records :: f (TableEntity GameRecordT)
     ,awaiters     :: f (TableEntity AwaiterT)
    }
  deriving (Generic, Database be)

lgsDb :: DatabaseSettings be LGSDb
lgsDb = defaultDbSettings
