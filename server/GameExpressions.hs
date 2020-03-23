{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GameExpressions where

import Data.Text (Text)
import Database.Beam
import Control.Monad
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Game
import qualified GameLogic as GL
import GameDB

dbFilename = "LGS.db"

getUser :: Int -> IO (Maybe User)
getUser userId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    mUser <- runSelectReturningOne $ lookup_ (_LGSUsers lgsDb) (UserId userId)
    pure mUser

--TODO: Hash password before storage
insertUser :: Text -> Text -> Text -> IO ()
insertUser userEmail userName userPassword = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsert $
      insert
        (_LGSUsers lgsDb)
        (insertExpressions
           [User default_ (val_ userEmail) (val_ userName) (val_ userPassword)])

getGameRecords :: Int -> IO [GameRecord]
getGameRecords playerId = do
  conn <- open dbFilename
  relatedGames <-
    runBeamSqlite conn $
    runSelectReturningList $
    select $ do
      user <- all_ (_LGSUsers lgsDb)
      gameRecord <- all_ (_LGSGameRecords lgsDb)
      guard_
        (_blackPlayer gameRecord `references_` user ||. _whitePlayer gameRecord `references_`
         user)
      pure gameRecord
  pure relatedGames


getGameRecord :: Int -> IO (Maybe GameRecord)
getGameRecord gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    mGame <- runSelectReturningOne $ lookup_ (_LGSGameRecords lgsDb) (GameRecordId gameId)
    pure mGame

insertGame :: Int -> Int -> Game -> IO ()
insertGame blackPlayer whitePlayer game = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsert $
      insert
        (_LGSGameRecords lgsDb)
        (insertExpressions
           [ GameRecord
               default_
               (val_ game)
               (val_ (UserId blackPlayer))
               (val_ (UserId whitePlayer))
           ])


updateGameProposal :: Int -> Bool -> IO (Maybe GameStatus)
updateGameProposal = updateProposal GL.updateGameProposal

updateCountingProposal :: Int -> Bool -> IO (Maybe GameStatus)
updateCountingProposal = updateProposal GL.updateCountingProposal

-- TODO: Add a check which computes and saves the final score once the territory has been accepted
updateTerritoryProposal ::  Int -> Bool -> IO (Maybe GameStatus)
updateTerritoryProposal = updateProposal GL.updateTerritoryProposal

updateGame :: (Game -> Game) -> Int -> IO (Maybe Game)
updateGame f gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    mGameRecord <- liftIO $ getGameRecord gameId
    case mGameRecord of
      Just gameRecord -> do
        let newGame = f (_game gameRecord)
        runUpdate (save (_LGSGameRecords lgsDb) (gameRecord {_game = newGame}))
        pure (Just newGame)
      Nothing -> pure Nothing

-- TODO: Field access is beginning to get unwieldly. Convert to lenses for DB types?
-- TODO: Should this return a Maybe GameStatus instead?
updateProposal ::
     (Bool -> Game -> Game)
  -> Int
  -> Bool
  -> IO (Maybe GameStatus)
updateProposal updateProposal gameId shouldCount = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    mGameRecord <- liftIO $ getGameRecord gameId
    case mGameRecord of
      Just gameRecord ->
        let oldGame = _game gameRecord
            oldStatus = _status oldGame
            newGame = updateProposal shouldCount oldGame
            newStatus = _status newGame
         in do when
                 (oldStatus /= newStatus)
                 (runUpdate
                    (save (_LGSGameRecords lgsDb) (gameRecord {_game = newGame})))
               pure (Just newStatus)
      Nothing -> pure Nothing
