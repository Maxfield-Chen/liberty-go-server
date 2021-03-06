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
import           Data.Functor
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Time           as Time
import qualified Game                as G
import qualified GameDB              as GDB
import           GHC.Generics
import           Servant.Auth.Server

data GameRecord =
  GameRecord
  {
    grId                     :: Int,
    grGame                   :: G.Game,
    grBlackPlayer            :: User,
    grWhitePlayer            :: User,
    grBlackTeacher           :: Maybe User,
    grWhiteTeacher           :: Maybe User,
    grBlackFocus             :: Text,
    grWhiteFocus             :: Text,
    grBlackGuidanceRemaining :: Int,
    grWhiteGuidanceRemaining :: Int
 } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)

data ChatMessage =
  ChatMessage
  {
    chatMessageSenderId   :: Int,
    chatMessageContent    :: Text,
    chatMessageGameId     :: Int,
    chatMessageSenderType :: GDB.UserType,
    chatMessageShared     :: Bool,
    chatMessageTimestamp  :: Time.LocalTime
  } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read)

newChatMessage = ChatMessage (-1) "" (-1) GDB.Watcher False
convertChatMessage :: GDB.ChatMessage -> ChatMessage
convertChatMessage GDB.ChatMessage{..} =
  let GDB.UserId senderId= _chat_message_sender_id
      GDB.GameRecordId gameId = _chat_message_game_id
  in ChatMessage
  senderId
  _chat_message_content
  gameId
  _chat_message_user_type
  _chat_message_shared
  _chat_message_timestamp


data GameUpdate =
  GameUpdate
  {
    guId   :: Int,
    guGame :: G.Game
  } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read)

newGameUpdate = GameUpdate (-1) G.newGame

newGameRecord = GameRecord (-1) G.newGame newUser newUser Nothing Nothing "" "" 3 3

userAwaiting :: (GameRecord -> User)
             -> GameRecord
             -> [Awaiter]
             -> Bool
userAwaiting f gr awaiters =
  (((==) G.GameProposed) . G._status . grGame) gr &&
    ((userId $ f gr) `elem` (fmap awaiterUser awaiters))

teacherAwaiting :: (GameRecord -> Maybe User)
             -> GameRecord
             -> [Awaiter]
             -> Maybe Bool
teacherAwaiting f gr awaiters =
  let gameProposed = (((==) G.GameProposed) . G._status . grGame) gr
  in ((&&) gameProposed) <$> (elem <$> (userId <$> f gr) <*> (Just $ fmap awaiterUser awaiters))

isWatcher :: User -> GameRecord -> Bool
isWatcher u GameRecord{..} =
  (not (u `elem` [grBlackPlayer, grWhitePlayer])) && not ( u `elem` catMaybes [grBlackTeacher, grWhiteTeacher])


isBlack :: User -> GameRecord -> Bool
isBlack u GameRecord{..} = grBlackPlayer == u

getOpponent :: User -> GameRecord -> User
getOpponent u g@GameRecord{..} = case isBlack u g of
  True  -> grWhitePlayer
  False -> grBlackPlayer

getTeacher :: User -> GameRecord -> Maybe User
getTeacher u g@GameRecord{..} = case isBlack u g of
  True  -> grBlackTeacher
  False -> grWhiteTeacher

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
    _black_guidance_remaining
    _white_guidance_remaining


data MarkedMove =
  MarkedMove
  {
    markedMoveTurnNumber :: Int
  , markedMoveUserId     :: Int
  , markedMoveGameId     :: Int
  , markedMoveOne        :: Maybe G.Position
  , markedMoveTwo        :: Maybe G.Position
  , markedMoveThree      :: Maybe G.Position
  } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)

convertMarkedMove :: GDB.MarkedMove ->MarkedMove
convertMarkedMove GDB.MarkedMove{..} =
  let (GDB.UserId userId) =_marked_move_user_id
      (GDB.GameRecordId gameId) =_marked_move_game_id
  in MarkedMove _marked_move_turn_number userId gameId _marked_move_one _marked_move_two _marked_move_three

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
    userImage :: Int,
    userEmail :: Text
  } deriving (Generic, ToJSON, FromJSON, Eq, Show, Read, ToJWT, FromJWT)

newUser = User (-1) "" 0 ""
convertUser :: GDB.User -> User
convertUser GDB.User{..} = User _userId _userName _userImage _userEmail

type AllGames = ([GameRecord], M.HashMap Int [Awaiter])

finishedGames :: AllGames -> [GameRecord]
finishedGames (gameRecords, _) = filter ((== G.TerritoryAccepted) . G._status . grGame) gameRecords

shouldShowMessages :: GDB.UserType -> Bool -> ChatMessage->  Bool
shouldShowMessages userType gameInProgress message =
   case (userType,gameInProgress) of
      (_, False) -> True
      (GDB.Watcher, True) -> chatMessageShared message
      (GDB.BlackPlayer,True) ->
        (chatMessageShared message ||
          chatMessageSenderType message == GDB.BlackPlayer ||
          chatMessageSenderType message == GDB.BlackTeacher)
      (GDB.BlackTeacher,True) ->
        (chatMessageShared message ||
          chatMessageSenderType message == GDB.BlackPlayer ||
          chatMessageSenderType message == GDB.BlackTeacher)
      (GDB.WhitePlayer,True) ->
        (chatMessageShared message ||
          chatMessageSenderType message == GDB.WhitePlayer ||
          chatMessageSenderType message == GDB.WhiteTeacher)
      (GDB.WhiteTeacher,True) ->
        (chatMessageShared message ||
          chatMessageSenderType message == GDB.WhitePlayer ||
          chatMessageSenderType message == GDB.WhiteTeacher)
