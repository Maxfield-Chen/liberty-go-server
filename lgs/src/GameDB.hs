{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


module GameDB where

import           Data.Aeson.Types
import           Data.Text                 (Text, unpack)
import qualified Data.Time                 as Time
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema
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

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = UserId . _userId

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
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show (PrimaryKey UserT (Nullable Identity))
deriving instance ToJSON (PrimaryKey UserT Identity)
deriving instance ToJSON (PrimaryKey UserT (Nullable Identity))

instance Table GameRecordT where
  data PrimaryKey GameRecordT f = GameRecordId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = GameRecordId . _gameId

type GameRecordId = PrimaryKey GameRecordT Identity

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Game where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite Game where
  fromBackendRow = read . unpack <$> fromBackendRow

data LGSDb f =
  LGSDb
    { _LGSUsers       :: f (TableEntity UserT)
     ,_LGSGameRecords :: f (TableEntity GameRecordT)
    }
  deriving (Generic, Database be)

lgsDb :: DatabaseSettings be LGSDb
lgsDb = defaultDbSettings
