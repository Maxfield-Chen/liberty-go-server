{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module GameExpressions where

import           Control.Monad
import           Data.Text              (Text)
import qualified Data.Time              as Time
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           Game
import           GameDB
import qualified GameLogic              as GL
import qualified UserInput

dbFilename = "LGS.db"

getUserViaCreds :: Text -> Text -> IO (Maybe User)
getUserViaCreds name pass = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
      user <- all_ (_users lgsDb)
      guard_ (_userName user ==. val_ name &&._userPasswordHash user ==. val_ pass)
      pure user

getUserViaName :: Text -> IO (Maybe User)
getUserViaName name = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
      user <- all_ (_users lgsDb)
      guard_ (_userName user ==. val_ name)
      pure user

getUser :: Int -> IO (Maybe User)
getUser userId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    mUser <- runSelectReturningOne $ lookup_ (_users lgsDb) (UserId userId)
    pure mUser

--TODO: Hash password before storage
insertUser :: Text -> Text -> Text -> IO ()
insertUser userEmail userName userPassword = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsert $
      insert
        (_users lgsDb)
        (insertExpressions
           [User default_ (val_ userEmail) (val_ userName) (val_ userPassword)])

getGameRecords :: Int -> IO [GameRecord]
getGameRecords playerId = do
  conn <- open dbFilename
  relatedGames <-
    runBeamSqlite conn $
    runSelectReturningList $
    select $ do
      user <- all_ (_users lgsDb)
      gameRecord <- all_ (_game_records lgsDb)
      guard_
        (_black_player gameRecord `references_` user ||. _white_player gameRecord `references_`
         user)
      pure gameRecord
  pure relatedGames

getGameRecord :: Int -> IO (Maybe GameRecord)
getGameRecord gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    mGame <- runSelectReturningOne $ lookup_ (_game_records lgsDb) (GameRecordId gameId)
    pure mGame

mPlayerIdToExpr mPlayerId = case mPlayerId of
  Just playerId -> just_ (val_ (UserId playerId))
  Nothing       -> nothing_

mTeacherIdToBool mTeacherId = case mTeacherId of
  Just teacherId -> just_ (val_ True)
  Nothing        -> nothing_

insertGame :: UserInput.ProposedGame -> Game -> IO ()
insertGame (UserInput.ProposedGame blackPlayer whitePlayer mBlackTeacher mWhiteTeacher blackFocus whiteFocus) game =
  do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsert $
      insert
        (_game_records lgsDb)
        (insertExpressions
           [ GameRecord
               default_
               (val_ game)
               (val_ (UserId blackPlayer))
               (val_ (UserId whitePlayer))
               (mPlayerIdToExpr mBlackTeacher)
               (mPlayerIdToExpr mWhiteTeacher)
               (val_ True)
               (val_ False)
               (mTeacherIdToBool mBlackTeacher)
               (mTeacherIdToBool mWhiteTeacher)
               (val_ blackFocus)
               (val_ whiteFocus)
               currentTimestamp_
           ])


updateGameProposal :: Int -> Bool -> IO (Maybe GameStatus)
updateGameProposal = updateProposal GL.updateGameProposal

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
        runUpdate (save (_game_records lgsDb) (gameRecord {_game = newGame}))
        pure (Just newGame)
      Nothing -> pure Nothing

-- TODO: Field access is beginning to get unwieldly. Convert to lenses for DB types?
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
            newgame = updateProposal shouldCount oldGame
            newStatus = _status newGame
         in do when
                 (oldStatus /= newStatus)
                 (runUpdate
                    (save (_game_records lgsDb) (gameRecord {_game = newGame})))
               pure (Just newStatus)
      Nothing -> pure Nothing
