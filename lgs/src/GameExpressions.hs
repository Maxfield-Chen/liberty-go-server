{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module GameExpressions where

import           Control.Lens           ((<&>))
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
  runBeamSqlite conn $ runSelectReturningOne $ lookup_ (_users lgsDb) (UserId userId)

isPlayerAwaiter :: Int -> Int -> IO Bool
isPlayerAwaiter playerId gameId =
  do
    conn <- open dbFilename
    awaiters <- runBeamSqlite conn $
      runSelectReturningList $
      select $ do
      awaiter <- all_ (_awaiters lgsDb)
      guard_ (_awaiter_user_id awaiter ==. val_ (UserId playerId)
             &&. _awaiter_game_id awaiter ==. val_ (GameRecordId gameId))
      pure awaiter
    pure (null awaiters)

getAwaiters :: Int -> IO [Awaiter]
getAwaiters gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningList $
    select $ do
    awaiter <- all_ (_awaiters (lgsDb))
    guard (_awaiter_game_id awaiter ==. val_ (GameRecordId gameId))
    pure awaiter

getGamePlayers :: Int -> IO [User]
getGamePlayers gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningList $
    select $ do
    gameRecord <- all_ (_game_records lgsDb)
    user <- all_ (_users lgsDb)
    guard_ (_gameId gameRecord ==. val_ gameId
           &&. (_black_player gameRecord `references_` user
           ||.  _white_player gameRecord `references_` user))
    pure user

getBlackPlayer :: Int -> IO (Maybe User)
getBlackPlayer gameId = referenceSingleUser _black_player gameId

getWhitePlayer :: Int -> IO (Maybe User)
getWhitePlayer gameId = referenceSingleUser _white_player gameId

getPlayerColor :: UserInput.User -> Int -> IO (Maybe Space)
getPlayerColor (UserInput.User _ name _) gameId = do
  mPlayer <- getUserViaName name
  mBlackPlayer <- getBlackPlayer gameId
  mWhitePlayer <- getWhitePlayer gameId
  pure $
    (==) <$> mPlayer <*> mBlackPlayer
    >>= (\case
            True -> Just Black
            False -> Just White)

referenceSingleUser f gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
    gameRecord <- all_ (_game_records lgsDb)
    user <- all_ (_users lgsDb)
    guard_ (_gameId gameRecord ==. val_ gameId
           &&. f gameRecord `references_` user)
    pure user

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
        (_userId user ==. val_ playerId
         &&. (_black_player gameRecord `references_` user
         ||.  _white_player gameRecord `references_` user))
      pure gameRecord
  pure relatedGames

getGameRecord :: Int -> IO (Maybe GameRecord)
getGameRecord gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $ runSelectReturningOne $ lookup_ (_game_records lgsDb) (GameRecordId gameId)

mPlayerIdToExpr mPlayerId = case mPlayerId of
  Just playerId -> just_ (val_ (UserId playerId))
  Nothing       -> nothing_

--TODO: Hash password before storage
insertUser :: Text -> Text -> Text -> IO [User]
insertUser userEmail userName userPassword = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
        (_users lgsDb)
        (insertExpressions
           [User default_ (val_ userEmail) (val_ userName) (val_ userPassword)])

insertAwaiter :: Int -> Int -> IO [Awaiter]
insertAwaiter gameId userId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
      (_awaiters lgsDb)
      (insertExpressions
        [Awaiter default_ (val_ (UserId userId )) (val_ (GameRecordId gameId ))])

insertGame :: UserInput.ProposedGame -> Game -> IO [GameRecord]
insertGame (UserInput.ProposedGame blackPlayer whitePlayer mBlackTeacher mWhiteTeacher blackFocus whiteFocus) game =
  do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
        (_game_records lgsDb)
        (insertExpressions
           [ GameRecord
               default_
               (val_ game)
               (val_ (UserId blackPlayer))
               (val_ (UserId whitePlayer))
               (mPlayerIdToExpr mBlackTeacher)
               (mPlayerIdToExpr mWhiteTeacher)
               (val_ blackFocus)
               (val_ whiteFocus)
               currentTimestamp_
           ])

updateGame :: (Game -> Game) -> Int -> IO (Maybe Game)
updateGame f gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    mGameRecord <- liftIO $ getGameRecord gameId
    case mGameRecord of
      Just gameRecord -> do
        let updatedGame = f (_game gameRecord)
        runUpdate (save (_game_records lgsDb) (gameRecord {_game = updatedGame}))
        pure (Just updatedGame)
      Nothing -> pure Nothing

deleteAwaiter :: Int -> Int -> IO ()
deleteAwaiter gameId playerId  = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runDelete $
      delete (_awaiters lgsDb)
             (\awaiter -> _awaiter_user_id awaiter ==. val_ (UserId playerId)
             &&. _awaiter_game_id awaiter ==. val_ (GameRecordId gameId ))
