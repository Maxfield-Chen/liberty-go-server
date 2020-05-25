{-# LANGUAGE OverloadedStrings #-}

module PubSubTypes where

import           Control.Concurrent.STM
import           Data.Aeson             (encode)
import           Data.Aeson.Types
import           Data.Function          (on)
import           Data.HashMap.Strict    as M
import qualified Data.Set               as Set
import           Data.Text
import           GameDB                 (GameRecord)
import qualified Network.WebSockets     as WS

type UserJWT = Text
type GameId = Int
type UserId = Int
type GameMap = TVar (M.HashMap GameId Game)

emptyGameMap :: IO GameMap
emptyGameMap = newTVarIO M.empty

data Game = Game { gameId      :: Int
                 , gameClients :: TVar (Set.Set Client)
                 , gameChan    :: TChan GameMessage}

data Client = Client { clientId        :: Int
                     , clientGames     :: TVar (Set.Set Game)
                     , clientGameChans :: TVar [TChan GameMessage]}

getGameChan = gameChan

instance Eq Client where
  (==) = on (==) clientId

instance Ord Client where
  compare = on compare clientId

instance Eq Game where
  (==) = on (==) gameId

instance Ord Game where
  compare = on compare gameId


data GameMessage = Join GameId UserJWT
                 | Leave GameId UserJWT
                 | Broadcast GameRecord

instance ToJSON GameMessage where
  toJSON (Join g jwt) =
    object ["type" .= ("join" :: Text)
          , "gameId" .= g
          , "jwt" .= jwt]
  toJSON (Leave g jwt) =
    object ["type" .= ("leave" :: Text)
          , "gameId" .= g
          , "jwt" .= jwt]
  toJSON (Broadcast gr) =
    object ["type" .= ("update" :: Text)
          , "gameRecord" .= gr]

instance WS.WebSocketsData GameMessage where
  fromLazyByteString = undefined
  toLazyByteString = encode
