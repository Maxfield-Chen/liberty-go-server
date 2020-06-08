{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PubSub where

import           Control.Concurrent.STM
import qualified Data.HashMap.Strict    as M
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T

import           PubSubTypes


getGame :: GameId -> GameMap -> STM Game
getGame gId gMap = do
  map <- readTVar gMap
  case M.lookup gId map of
    Just game -> pure game
    Nothing -> do
      game <- makeEmptyGame gId
      modifyTVar gMap $ M.insert gId game
      pure game

makeEmptyGame :: GameId -> STM Game
makeEmptyGame gId = do
  chan <- newBroadcastTChan
  clients <- newTVar Set.empty
  pure $ Game gId clients chan

makeNewClient :: UserName -> STM Client
makeNewClient uName = do
  games <- newTVar Set.empty
  gameChans <- newTVar []
  pure $ Client uName games gameChans

subscribe :: Client -> Game -> STM ()
subscribe client@Client{..} game@Game{..} = do
  gameSet <- readTVar clientGames
  newGameChan <- dupTChan gameChan
  let newGameSet = Set.insert (game {gameChan = newGameChan}) gameSet
  writeTVar clientGameChans $ map getGameChan (Set.toAscList newGameSet)
  modifyTVar gameClients $ Set.insert client

leave :: Client -> Game -> STM ()
leave client@Client{..} game@Game{..} = do
  gameSet <- readTVar clientGames
  let newGameSet = Set.delete game gameSet
  writeTVar clientGames newGameSet
  writeTVar clientGameChans $ map getGameChan (Set.toAscList newGameSet)
  modifyTVar gameClients $ Set.insert client

getAvailableMessage :: Client -> STM GameMessage
getAvailableMessage Client{..} = do
  clientGameChans <- readTVar clientGameChans
  foldl orElse retry $ map readTChan clientGameChans

clientSubbed :: Client -> Game -> STM Bool
clientSubbed Client{..} game = do
  clientGames <- readTVar clientGames
  pure $ Set.member game clientGames

allClientsSubbed :: Game -> STM [Client]
allClientsSubbed Game{..} = do
  gameClients <- readTVar gameClients
  pure $ Set.toAscList gameClients

allGamesSubbed :: Client -> STM [Game]
allGamesSubbed Client{..} = do
  clientGames <- readTVar clientGames
  pure $ Set.toAscList clientGames
