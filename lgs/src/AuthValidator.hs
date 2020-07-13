{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module AuthValidator where

import           Config
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans  (liftIO)
import           Data.Maybe
import           Data.Text            (Text)
import qualified Game                 as G
import qualified GameDB               as GDB hiding (User)
import qualified GameExpressions      as GEX
import qualified GameLogic            as GL
import qualified OutputTypes          as OT
import           Proofs
import           Servant
import           Servant.Auth.Server
import           Servant.Server
import           Theory.Named
import qualified UserInput

type AppM = ReaderT Config Handler

placeStone :: UserInput.User -> Int -> AppM ()
placeStone user gameId = errPlayerExcluded user gameId >>
  noAwaiters gameId


proposeGame :: UserInput.User -> UserInput.ProposedGame -> AppM ()
proposeGame (UserInput.User _ _ _ id) (UserInput.ProposedGame bp wp bt wt _ _) =
  do
    config <- ask
    mUser <- liftIO $ runReaderT  (GEX.getUser id) config
    case mUser of
      Nothing -> throwError err410
      Just user -> do
        let callingUserIncluded =
              foldr
                    (\uid t -> t || uid == GDB._userId user)
                    False
                    [bp, wp, fromMaybe (-1) bt,  fromMaybe (-1) wt]
        when (not callingUserIncluded) $ throwError err401

acceptGameProposal :: UserInput.User -> Int -> AppM ()
acceptGameProposal (UserInput.User _ _ _ id) gameId =
  do
    config <- ask
    mUser <- liftIO $ runReaderT (GEX.getUser id) config
    case mUser of
      Nothing -> throwError err410
      Just user ->
        do
          invalidUser <- liftIO $ not <$> runReaderT (GEX.isPlayerAwaiter (GDB._userId user) gameId) config
          when invalidUser $ throwError err401


noAwaiters :: Int -> AppM ()
noAwaiters gameId = do
  config <- ask
  awaiters <- liftIO $ runReaderT (GEX.getAwaiters gameId) config
  when (not $ null awaiters) $ throwError err401

proposePass :: UserInput.User -> Int -> AppM ()
proposePass user gameId = errPlayerExcluded user gameId >>
  noAwaiters gameId

proposeTerritory :: UserInput.User -> Int -> AppM ()
proposeTerritory user gameId = errPlayerExcluded user gameId >> noAwaiters gameId

acceptTerritoryProposal :: UserInput.User -> Int -> AppM ()
acceptTerritoryProposal user gameId = errPlayerExcluded user gameId >> noAwaiters gameId

markMove :: UserInput.User -> Int -> AppM (Maybe OT.GameRecord)
markMove = errUserExcluded


errUserExcluded :: UserInput.User -> Int -> AppM (Maybe OT.GameRecord)
errUserExcluded (UserInput.User _ _ _ id) gameId =
  do
    config <- ask
    mBaseGameRecord <- liftIO $ runReaderT (GEX.getGameRecord gameId) config
    case mBaseGameRecord of
      Nothing -> throwError err410
      Just baseGameRecord -> do
        mGameRecord <- liftIO $ runReaderT (GEX.convertDeepGameRecord baseGameRecord) config
        let players = OT.userId <$> catMaybes [OT.grBlackPlayer <$> mGameRecord
                                , OT.grWhitePlayer <$> mGameRecord
                                , OT.grBlackTeacher =<< mGameRecord
                                , OT.grWhiteTeacher =<< mGameRecord]
        case id `elem` players of
          False -> throwError err401
          _     -> pure mGameRecord

matchUserId :: GDB.UserId -> Int
matchUserId (GDB.UserId ret) = ret

errPlayerExcluded :: UserInput.User -> Int -> AppM ()
errPlayerExcluded (UserInput.User _ _ _ id) gameId =
  do
    config <- ask
    mUser <-  liftIO $ runReaderT (GEX.getUser id) config
    players <- liftIO $ runReaderT (GEX.getGamePlayers gameId) config
    case elem <$> mUser <*> Just players of
      Nothing    -> throwError err410
      Just False -> throwError err401
      _          -> pure ()

sendMessage :: UserInput.User ->  Int -> Bool -> AppM (GDB.UserType)
sendMessage (UserInput.User _ _ _ userId) gameId shared = do
  config <- ask
  userType <- liftIO $ liftIO $ runReaderT (GEX.getUserType userId gameId) config
  case (userType, shared) of
    (GDB.Watcher, True) -> throwError err401
    _                   -> pure userType
