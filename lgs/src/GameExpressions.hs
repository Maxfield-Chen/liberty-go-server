{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE  RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module GameExpressions where

import           Control.Lens
import           Control.Monad
import           Data.Text              (Text)
import           Data.Maybe
import qualified Data.Time              as Time
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           Debug.Trace
import qualified Game                   as G
import           GameDB                 (awaiter_game_id, awaiter_id,
                                         awaiter_user_id, blackFocus,
                                         blackPlayer, blackTeacher, game, grId,
                                         lgsDb, timestamp, userEmail, userId,
                                         userName, userPasswordHash, whiteFocus,
                                         GameRecord, UserType, UserId)
import qualified GameDB                 as GDB
import qualified GameLogic              as GL
import qualified UserInput
import           Control.Monad.Reader
import Config
import Servant


type AppM = ReaderT Config Handler

getUserViaCreds ::  Text -> Text ->  AppM (Maybe GDB.User)
getUserViaCreds name pass = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
      user <- all_ (GDB.users lgsDb)
      guard_ (user ^. userName ==. val_ name &&. user ^. userPasswordHash ==. val_ pass)
      pure user

getUserViaName ::  Text ->  AppM (Maybe GDB.User)
getUserViaName name = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
      user <- all_ (GDB.users lgsDb)
      guard_ (user ^. userName ==. val_ name)
      pure user

getUser ::  Int ->  AppM (Maybe GDB.User)
getUser userId = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $ runSelectReturningOne $ lookup_ (GDB.users lgsDb) (GDB.UserId userId)

isPlayerAwaiter ::  Int -> Int ->  AppM Bool
isPlayerAwaiter playerId gameId = do
  conn <- asks dbConnection
  awaiters <- liftIO $ runBeamSqlite conn $
    runSelectReturningList $
    select $ do
    awaiter <- all_ (GDB.awaiters lgsDb)
    guard_ (GDB._awaiter_user_id awaiter ==. val_ (GDB.UserId playerId)
            &&. GDB._awaiter_game_id awaiter ==. val_ (GDB.GameRecordId gameId))
    pure awaiter
  pure (not $ null awaiters)

getAwaiters ::  Int ->  AppM [GDB.Awaiter]
getAwaiters gameId = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $
    runSelectReturningList $
    select $ do
    awaiter <- all_ (GDB.awaiters lgsDb)
    gameRecord <- all_ (GDB.game_records (lgsDb))
    guard_ (GDB._gameId gameRecord ==. val_ gameId
           &&. GDB._awaiter_game_id awaiter `references_` gameRecord)
    pure awaiter

getGamePlayers ::  Int ->  AppM [GDB.User]
getGamePlayers gameId = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $
    runSelectReturningList $
    select $ do
    gameRecord <- all_ (GDB.game_records lgsDb)
    user <- all_ (GDB.users lgsDb)
    guard_ (GDB._gameId gameRecord ==. val_ gameId
           &&. (GDB._black_player gameRecord `references_` user
           ||.  GDB._white_player gameRecord `references_` user))
    pure user

getBlackPlayer ::  Int ->  AppM (Maybe GDB.User)
getBlackPlayer gameId = do
  conn <- asks dbConnection
  liftIO $ referenceSingleUser conn GDB._black_player gameId

getWhitePlayer ::  Int ->  AppM (Maybe GDB.User)
getWhitePlayer gameId = do
  conn <- asks dbConnection
  liftIO $ referenceSingleUser conn GDB._white_player gameId


getPlayerColor ::  UserInput.User -> Int ->  AppM (Maybe G.Space)
getPlayerColor (UserInput.User _ name _) gameId = do
  conn <- asks dbConnection
  mPlayer <- getUserViaName name
  mBlackPlayer <- getBlackPlayer gameId
  mWhitePlayer <- getWhitePlayer gameId
  pure $
    (==) <$> mPlayer <*> mBlackPlayer
    >>= (\case
            True -> Just G.Black
            False -> Just G.White)

referenceSingleUser conn f gameId = do
  runBeamSqlite conn $
    runSelectReturningOne $
    select $ do
    gameRecord <- all_ (GDB.game_records lgsDb)
    user <- all_ (GDB.users lgsDb)
    guard_ (GDB._gameId gameRecord ==. val_ gameId
           &&. f gameRecord `references_` user)
    pure user

-- TODO : Return if teacher matches as well
getPlayersGameRecords ::  Int ->  AppM [GDB.GameRecord]
getPlayersGameRecords playerId = do
  conn <- asks dbConnection
  relatedGames <-liftIO $
    runBeamSqlite conn $
    runSelectReturningList $
    select $ do
      user <- all_ (GDB.users lgsDb)
      gameRecord <- all_ (GDB.game_records lgsDb)
      guard_
        (GDB._userId user ==. val_ playerId
         &&. (GDB._black_player gameRecord `references_` user
         ||.  GDB._white_player gameRecord `references_` user
         ||.  (maybe_ (val_ False) (\bt -> bt `references_` user) (GDB._black_teacher gameRecord))
         ||.  (maybe_ (val_ False) (\wt -> wt `references_` user) (GDB._white_teacher gameRecord))))
      pure gameRecord
  pure relatedGames

getUserType :: Int -> Int ->  AppM GDB.UserType
getUserType senderId gameId = do
  mGameRecord <- getGameRecord gameId
  pure . fromMaybe GDB.Watcher $ ((\GDB.GameRecord{..} ->
          let  GDB.UserId bp = _black_player
               GDB.UserId wp = _white_player
               GDB.UserId bt = _black_teacher
               GDB.UserId wt = _white_teacher
          in fmap snd . listToMaybe $ filter ((==) senderId . fst)
             [(bp, GDB.BlackPlayer),
              (wp, GDB.WhitePlayer),
              (fromMaybe (-1) bt, GDB.BlackPlayer),
              (fromMaybe (-1) wt, GDB.WhiteTeacher)]
       ) =<< mGameRecord)


getGameRecord ::  Int ->  AppM (Maybe GDB.GameRecord)
getGameRecord gameId = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $ runSelectReturningOne $ lookup_ (GDB.game_records lgsDb) (GDB.GameRecordId gameId)

mPlayerIdToExpr mPlayerId = case mPlayerId of
  Just playerId -> just_ (val_ (GDB.UserId playerId))
  Nothing       -> nothing_

--TODO: Hash password before storage
insertUser ::  Text -> Text -> Text ->  AppM [GDB.User]
insertUser userEmail userName userPassword = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
        (GDB.users lgsDb)
        (insertExpressions
           [GDB.User default_ (val_ userEmail) (val_ userName) (val_ userPassword)])

insertAwaiter ::  Int -> Int ->  AppM [GDB.Awaiter]
insertAwaiter gameId userId = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
      (GDB.awaiters lgsDb)
      (insertExpressions
        [GDB.Awaiter default_ (val_ (GDB.UserId userId )) (val_ (GDB.GameRecordId gameId ))])

insertGame ::  UserInput.ProposedGame -> G.Game ->  AppM [GDB.GameRecord]
insertGame (UserInput.ProposedGame blackPlayer whitePlayer mBlackTeacher mWhiteTeacher blackFocus whiteFocus) game = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
        (GDB.game_records lgsDb)
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

updateGame ::  (G.Game -> G.Game) -> Int ->  AppM (Maybe G.Game)
updateGame f gameId = do
  conn <- asks dbConnection
  mGameRecord <- getGameRecord gameId
  liftIO $ runBeamSqlite conn $ do
    case mGameRecord of
      Just gameRecord -> do
        let updatedGame = f (gameRecord ^. game)
        runUpdate (save (GDB.game_records lgsDb) (gameRecord & game .~ updatedGame))
        pure (Just updatedGame)
      Nothing -> pure Nothing

deleteAwaiter ::  Int -> Int ->  AppM ()
deleteAwaiter gameId playerId  = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $ do
    runDelete $
      delete (GDB.awaiters lgsDb)
             (\awaiter -> GDB._awaiter_user_id awaiter ==. val_ (GDB.UserId playerId)
             &&. GDB._awaiter_game_id awaiter ==. val_ (GDB.GameRecordId gameId ))

insertChatMessage :: Int -> Text -> Bool -> Int ->  AppM [ GDB.ChatMessage]
insertChatMessage senderId content shared gameId = do
  conn <- asks dbConnection
  userType <- getUserType senderId gameId
  liftIO $ runBeamSqlite conn $ do
    runInsertReturningList $
      insertReturning
        (GDB.chat_messages lgsDb)
        (insertExpressions
          [GDB.ChatMessage
            default_
            (val_(  GDB.UserId senderId))
            (val_ content )
            (val_ userType )
            (val_ (GDB.GameRecordId gameId))
            (val_ shared)
            currentTimestamp_
          ])

getMessages :: Int -> AppM [ GDB.ChatMessage ]
getMessages gameId = do
  conn <- asks dbConnection
  liftIO $ runBeamSqlite conn $
    runSelectReturningList $
    select $ do
    message <- all_ (GDB.chat_messages lgsDb)
    guard_ (GDB._chat_message_game_id message ==. val_ (GDB.GameRecordId gameId))
    pure message
