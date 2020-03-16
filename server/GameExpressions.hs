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
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Game
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

-- TODO: Field access is beginning to get unwieldly. Convert to lenses for DB types?
updateCountingStatus :: Bool -> Int -> IO (Either MoveError GameStatus)
updateCountingStatus shouldCount gameId = do
  conn <- liftIO $ open dbFilename
  runBeamSqlite conn $ do
    mGameRecord <- liftIO $ gameIdToGameRecord gameId
    case mGameRecord of
      Just gameRecord ->
        let game = _game gameRecord
            oldStatus = _status game
            getNewStatus old req
              | old == GameProposed ||
                  old == OutcomeProposed || old == OutcomeAccepted = old
              | old == InProgress && req = CountingProposed
              | old == CountingProposed && req = CountingAccepted
              | otherwise = InProgress
         in do runUpdate $
                 save
                   (_LGSGameRecords lgsDb)
                   (gameRecord
                      { _game =
                          game
                            {_status = getNewStatus (_status game) shouldCount}
                      })
               pure (Right CountingProposed)
      Nothing -> pure (Left NoBoard)
