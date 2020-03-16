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

--TODO: Put additional thought into naming conventions, not a huge fan
--      of how overly verbose these are becoming.

userIdToUser :: Int -> IO (Maybe User)
userIdToUser userId = do
  conn <- liftIO $ open dbFilename
  runBeamSqlite conn $ do
    mUser <- runSelectReturningOne $ lookup_ (_LGSUsers lgsDb) (UserId userId)
    pure mUser

--TODO: Hash password before storage
insertUser :: Text -> Text -> Text -> IO ()
insertUser userEmail userName userPassword = do
  conn <- liftIO $ open dbFilename
  runBeamSqlite conn $ do
    runInsert $
      insert
        (_LGSUsers lgsDb)
        (insertExpressions
           [User default_ (val_ userEmail) (val_ userName) (val_ userPassword)])


gameIdToGameRecord :: Int -> IO (Maybe GameRecord)
gameIdToGameRecord gameId = do
  conn <- liftIO $ open dbFilename
  runBeamSqlite conn $ do
    mGame <- runSelectReturningOne $ lookup_ (_LGSGameRecords lgsDb) (GameRecordId gameId)
    pure mGame

insertGame :: Int -> Int -> Game -> IO ()
insertGame blackPlayer whitePlayer game = do
  conn <- liftIO $ open dbFilename
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

updateCountingProposal :: Int -> Bool -> IO (Either MoveError GameStatus)
updateCountingProposal = updateProposal GL.updateCountingProposal

updateTerritoryProposal :: Territory -> Int -> Bool -> IO (Either MoveError GameStatus)
updateTerritoryProposal territory = updateProposal (GL.updateTerritoryProposal territory)

-- TODO: Field access is beginning to get unwieldly. Convert to lenses for DB types?
updateProposal ::
     (Bool -> Game -> Game)
  -> Int
  -> Bool
  -> IO (Either MoveError GameStatus)
updateProposal updateProposal gameId shouldCount = do
  conn <- liftIO $ open dbFilename
  runBeamSqlite conn $ do
    mGameRecord <- liftIO $ gameIdToGameRecord gameId
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
               pure (Right newStatus)
      Nothing -> pure (Left NoBoard)
