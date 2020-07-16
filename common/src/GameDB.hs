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
import qualified Data.HashMap.Strict       as M
import           Data.Text                 (Text, unpack)
import qualified Data.Time                 as Time
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite
import           Game
import           Servant.Auth.Server

data ProfileImage =
   None |
   Bull |
   Cow |
   Donkey |
   Duck |
   Goat |
   Goose |
   Horse |
   Pig |
   Rabbit |
   Ram |
   Rooster |
   Sheep deriving (Show, Eq, Enum, Bounded)

data UserT f
  = User {_userId           :: Columnar f Int
         ,_userEmail        :: Columnar f Text
         ,_userName         :: Columnar f Text
         ,_userImage        :: Columnar f Int
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
deriving instance Eq (PrimaryKey UserT (Nullable Identity))
deriving instance ToJSON (PrimaryKey UserT Identity)
deriving instance ToJSON (PrimaryKey UserT (Nullable Identity))
deriving instance FromJSON (PrimaryKey UserT Identity)
deriving instance FromJSON (PrimaryKey UserT (Nullable Identity))

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = UserId . _userId

User (LensFor userId) (LensFor userEmail)
     (LensFor userName) (LensFor userImage) (LensFor userPasswordHash)
     = tableLenses


data UserType= BlackPlayer |WhitePlayer|BlackTeacher|WhiteTeacher|Watcher deriving(Show, Eq, ToJSON, FromJSON, Generic, Read)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be   UserType where
  sqlValueSyntax = autoSqlValueSyntax

data ChatMessageT f
  = ChatMessage {_chat_message_id    :: Columnar f Int
            ,_chat_message_sender_id :: PrimaryKey UserT f
            ,_chat_message_content   :: Columnar f Text
            ,_chat_message_user_type :: Columnar f  UserType
            ,_chat_message_game_id   :: PrimaryKey GameRecordT f
            ,_chat_message_shared    :: Columnar f  Bool
            ,_chat_message_timestamp :: Columnar f Time.LocalTime
            } deriving (Generic, Beamable)

type ChatMessage = ChatMessageT Identity
type ChatMessageId = PrimaryKey ChatMessageT Identity

deriving instance Eq ChatMessage
deriving instance Show ChatMessage
deriving instance ToJSON ChatMessage
deriving instance FromJSON ChatMessage

deriving instance Show (PrimaryKey ChatMessageT Identity)
deriving instance ToJSON (PrimaryKey ChatMessageT Identity)
deriving instance FromJSON (PrimaryKey ChatMessageT Identity)

instance Table ChatMessageT where
  data PrimaryKey ChatMessageT f = ChatMessageId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = ChatMessageId . _chat_message_id


ChatMessage (LensFor chat_message_id) (UserId (LensFor chat_message_sender_id)) (LensFor chat_message_content) ( LensFor chat_message_user_type) (GameRecordId (LensFor chat_message_game_id)) ( LensFor chat_message_shared)( LensFor chat_timestamp)= tableLenses

instance HasSqlValueSyntax be String => HasSqlValueSyntax be  ChatMessage where
  sqlValueSyntax = autoSqlValueSyntax

data MarkedMoveT f
  = MarkedMove {_marked_move_id       :: Columnar f Int
            ,_marked_move_turn_number :: Columnar f Int
            ,_marked_move_user_id     :: PrimaryKey UserT f
            ,_marked_move_game_id     :: PrimaryKey GameRecordT f
            , _marked_move_one        :: Columnar (Nullable f) Int
            , _marked_move_two        :: Columnar (Nullable f) Int
            , _marked_move_three      :: Columnar (Nullable f) Int
            } deriving (Generic, Beamable)

type MarkedMove = MarkedMoveT Identity
type MarkedMoveId = PrimaryKey MarkedMoveT Identity

deriving instance Eq MarkedMove
deriving instance Show MarkedMove
deriving instance ToJSON MarkedMove
deriving instance FromJSON MarkedMove

deriving instance Show (PrimaryKey MarkedMoveT Identity)
deriving instance ToJSON (PrimaryKey MarkedMoveT Identity)
deriving instance FromJSON (PrimaryKey MarkedMoveT Identity)

instance Table MarkedMoveT where
  data PrimaryKey MarkedMoveT f = MarkedMoveId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = MarkedMoveId . _marked_move_id


MarkedMove (LensFor markedMoveId) (LensFor markedMoveTurnNumber)
        (UserId (LensFor markedMoveUserId)) (GameRecordId (LensFor marked_move_game_id))
        (LensFor markedMoveOne) (LensFor markedMoveTwo) (LensFor markedMoveThree)
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
  = GameRecord {_gameId                   :: Columnar f Int
               ,_game                     :: Columnar f Game
               ,_black_player             :: PrimaryKey UserT f
               ,_white_player             :: PrimaryKey UserT f
               ,_black_teacher            :: PrimaryKey UserT (Nullable f)
               ,_white_teacher            :: PrimaryKey UserT (Nullable f)
               ,_black_focus              :: Columnar f Text
               ,_white_focus              :: Columnar f Text
               ,_black_guidance_remaining :: Columnar f Int
               ,_white_guidance_remaining :: Columnar f Int
               ,_timestamp                :: Columnar f Time.LocalTime
               } deriving (Generic, Beamable)

type GameRecord = GameRecordT Identity
deriving instance Show GameRecord
deriving instance Eq GameRecord
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
           (LensFor blackGuidanceRemaining) (LensFor whiteGuidanceRemaining)
           (LensFor timestamp)
           = tableLenses

-- LGL SQL Representation Definitions
instance HasSqlValueSyntax be String => HasSqlValueSyntax be Game where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite Game where
  fromBackendRow = read . unpack <$> fromBackendRow

data LGSDb f =
  LGSDb
    { users         :: f (TableEntity UserT)
     ,game_records  :: f (TableEntity GameRecordT)
     ,awaiters      :: f (TableEntity AwaiterT)
     ,chat_messages :: f (TableEntity ChatMessageT)
     ,marked_moves  :: f (TableEntity MarkedMoveT)
    }
  deriving (Generic, Database be)

lgsDb :: DatabaseSettings be LGSDb
lgsDb = defaultDbSettings

instance FromBackendRow Sqlite  UserType where
  fromBackendRow = read . unpack <$> fromBackendRow
