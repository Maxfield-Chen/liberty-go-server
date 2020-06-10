{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module PubSubTypes where

import           Control.Concurrent.STM
import           Data.Aeson             (decode, eitherDecode, encode)
import           Data.Aeson.Types
import           Data.Function          (on)
import           Data.HashMap.Strict    as M
import qualified Data.Set               as Set
import           Data.Text
import qualified Game                   as G
import qualified Network.WebSockets     as WS

type UserJWT = Text
type GameId = Int
type UserName = Text
type GameMap = TVar (M.HashMap GameId Game)

emptyGameMap :: IO GameMap
emptyGameMap = newTVarIO M.empty

data Game = Game { gameId      :: Int
                 , gameClients :: TVar (Set.Set Client)
                 , gameChan    :: TChan GameMessage}

data Client = Client { clientName      :: UserName
                     , clientGames     :: TVar (Set.Set Game)
                     , clientGameChans :: TVar [TChan GameMessage]}

getGameChan = gameChan

instance Eq Client where
  (==) = on (==) clientName

instance Ord Client where
  compare = on compare clientName

instance Eq Game where
  (==) = on (==) gameId

instance Ord Game where
  compare = on compare gameId

data ErrorMessage = ParserError Text
              | InitialConnectionError Text
              | ForbiddenJoinError Text
              deriving Show

instance ToJSON ErrorMessage where
    toJSON (ParserError t) =
        object [ "type" .= ("error" :: Text)
               , "code" .= (1 :: Int)
               , "payload" .= t]
    toJSON (InitialConnectionError t) =
        object [ "type" .= ("error" :: Text)
               , "code" .= (2 :: Int)
               , "payload" .= t]
    toJSON (ForbiddenJoinError t) =
        object [ "type" .= ("error" :: Text)
               , "code" .= (3 :: Int)
               , "payload" .= t]

data GameMessage = UpdateGame G.Game

data IncomingMessage = JoinGame GameId
                     | LeaveGame GameId

instance ToJSON GameMessage where
  toJSON (UpdateGame g) =
    object ["type" .= ("update" :: Text)
          , "game" .= g]

instance WS.WebSocketsData GameMessage where
  fromLazyByteString = undefined
  toLazyByteString = encode

instance ToJSON IncomingMessage where
  toJSON (JoinGame g ) =
    object ["type" .= ("join" :: Text)
          , "gameId" .= g]
  toJSON (LeaveGame g ) =
    object ["type" .= ("leave" :: Text)
          , "gameId" .= g]

instance FromJSON IncomingMessage where
  parseJSON o@(Object v) = do
    typ <- v .: "type" :: Parser Text
    case typ of
      "join"  -> JoinGame <$> v.: "gameId"
      "leave" -> LeaveGame <$> v.: "gameId"
      _       -> typeMismatch "Invalid Message Type" o
  parseJSON invalid = typeMismatch "Invalid JSON" invalid

instance WS.WebSocketsData (Either ErrorMessage IncomingMessage) where
  fromLazyByteString s =
    case eitherDecode s of
      Left m  -> Left . ParserError $ pack m
      Right i -> Right i
  toLazyByteString = encode
  fromDataMessage (WS.Text s _) =
    maybe (Left $ ParserError "Unable to decode UTF8 WS MSG.") Right (decode s)
  fromDataMessage (WS.Binary bMsg) = WS.fromLazyByteString bMsg

instance WS.WebSocketsData ErrorMessage where
  fromLazyByteString = undefined
  toLazyByteString = encode
