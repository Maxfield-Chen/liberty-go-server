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
import qualified Game                   as G
import           GameDB                 (lgsDb)
import qualified GameDB                 as GDB
import qualified GameLogic              as GL
import qualified UserInput

dbFilename = "LGS.db"

getUserViaCreds :: Text -> Text -> IO (Maybe GDB.User)
getUserViaCreds name pass = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
      user <- all_ (GDB._users lgsDb)
      guard_ (GDB._userName user ==. val_ name &&. GDB._userPasswordHash user ==. val_ pass)
      pure user

getUserViaName :: Text -> IO (Maybe GDB.User)
getUserViaName name = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
      user <- all_ (GDB._users lgsDb)
      guard_ (GDB._userName user ==. val_ name)
      pure user

getUser :: Int -> IO (Maybe GDB.User)
getUser userId = do
  conn <- open dbFilename
  runBeamSqlite conn $ runSelectReturningOne $ lookup_ (GDB._users lgsDb) (GDB.UserId userId)

isPlayerAwaiter :: Int -> Int -> IO Bool
isPlayerAwaiter playerId gameId =
  do
    conn <- open dbFilename
    awaiters <- runBeamSqlite conn $
      runSelectReturningList $
      select $ do
      awaiter <- all_ (GDB._awaiters lgsDb)
      guard_ (GDB._awaiter_user_id awaiter ==. val_ (GDB.UserId playerId)
             &&. GDB._awaiter_game_id awaiter ==. val_ (GDB.GameRecordId gameId))
      pure awaiter
    pure (not $ null awaiters)

getAwaiters :: Int -> IO [GDB.Awaiter]
getAwaiters gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningList $
    select $ do
    awaiter <- all_ (GDB._awaiters lgsDb)
    gameRecord <- all_ (GDB._game_records (lgsDb))
    guard_ (GDB._gameId gameRecord ==. val_ gameId
           &&. GDB._awaiter_game_id awaiter `references_` gameRecord)
    pure awaiter

getGamePlayers :: Int -> IO [GDB.User]
getGamePlayers gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningList $
    select $ do
    gameRecord <- all_ (GDB._game_records lgsDb)
    user <- all_ (GDB._users lgsDb)
    guard_ (GDB._gameId gameRecord ==. val_ gameId
           &&. (GDB._black_player gameRecord `references_` user
           ||.  GDB._white_player gameRecord `references_` user))
    pure user

getBlackPlayer :: Int -> IO (Maybe GDB.User)
getBlackPlayer gameId = referenceSingleUser GDB._black_player gameId

getWhitePlayer :: Int -> IO (Maybe GDB.User)
getWhitePlayer gameId = referenceSingleUser GDB._white_player gameId

getPlayerColor :: UserInput.User -> Int -> IO (Maybe G.Space)
getPlayerColor (UserInput.User _ name _) gameId = do
  mPlayer <- getUserViaName name
  mBlackPlayer <- getBlackPlayer gameId
  mWhitePlayer <- getWhitePlayer gameId
  pure $
    (==) <$> mPlayer <*> mBlackPlayer
    >>= (\case
            True -> Just G.Black
            False -> Just G.White)

referenceSingleUser f gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
    gameRecord <- all_ (GDB._game_records lgsDb)
    user <- all_ (GDB._users lgsDb)
    guard_ (GDB._gameId gameRecord ==. val_ gameId
           &&. f gameRecord `references_` user)
    pure user

getGameRecords :: Int -> IO [GDB.GameRecord]
getGameRecords playerId = do
  conn <- open dbFilename
  relatedGames <-
    runBeamSqlite conn $
    runSelectReturningList $
    select $ do
      user <- all_ (GDB._users lgsDb)
      gameRecord <- all_ (GDB._game_records lgsDb)
      guard_
        (GDB._userId user ==. val_ playerId
         &&. (GDB._black_player gameRecord `references_` user
         ||.  GDB._white_player gameRecord `references_` user))
      pure gameRecord
  pure relatedGames

getGameRecord :: Int -> IO (Maybe GDB.GameRecord)
getGameRecord gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $ runSelectReturningOne $ lookup_ (GDB._game_records lgsDb) (GDB.GameRecordId gameId)

mPlayerIdToExpr mPlayerId = case mPlayerId of
  Just playerId -> just_ (val_ (GDB.UserId playerId))
  Nothing       -> nothing_

--TODO: Hash password before storage
insertUser :: Text -> Text -> Text -> IO [GDB.User]
insertUser userEmail userName userPassword = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
        (GDB._users lgsDb)
        (insertExpressions
           [GDB.User default_ (val_ userEmail) (val_ userName) (val_ userPassword)])

insertAwaiter :: Int -> Int -> IO [GDB.Awaiter]
insertAwaiter gameId userId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
      (GDB._awaiters lgsDb)
      (insertExpressions
        [GDB.Awaiter default_ (val_ (GDB.UserId userId )) (val_ (GDB.GameRecordId gameId ))])

insertGame :: UserInput.ProposedGame -> G.Game -> IO [GDB.GameRecord]
insertGame (UserInput.ProposedGame blackPlayer whitePlayer mBlackTeacher mWhiteTeacher blackFocus whiteFocus) game =
  do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
        (GDB._game_records lgsDb)
        (insertExpressions
           [ GDB.GameRecord
               default_
               (val_ game)
               (val_ (GDB.UserId blackPlayer))
               (val_ (GDB.UserId whitePlayer))
               (mPlayerIdToExpr mBlackTeacher)
               (mPlayerIdToExpr mWhiteTeacher)
               (val_ blackFocus)
               (val_ whiteFocus)
               currentTimestamp_
           ])

updateGame :: (G.Game -> G.Game) -> Int -> IO (Maybe G.Game)
updateGame f gameId = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    mGameRecord <- liftIO $ getGameRecord gameId
    case mGameRecord of
      Just gameRecord -> do
        let updatedGame = f (GDB._game gameRecord)
        runUpdate (save (GDB._game_records lgsDb) (gameRecord {GDB._game = updatedGame}))
        pure (Just updatedGame)
      Nothing -> pure Nothing

deleteAwaiter :: Int -> Int -> IO ()
deleteAwaiter gameId playerId  = do
  conn <- open dbFilename
  runBeamSqlite conn $ do
    runDelete $
      delete (GDB._awaiters lgsDb)
             (\awaiter -> GDB._awaiter_user_id awaiter ==. val_ (GDB.UserId playerId)
             &&. GDB._awaiter_game_id awaiter ==. val_ (GDB.GameRecordId gameId ))
