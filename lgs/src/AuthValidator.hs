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

import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Data.Maybe
import           Data.Text           (Text)
import qualified Game                as G
import qualified GameDB              as GDB hiding (User)
import qualified GameExpressions     as GEX
import qualified GameLogic           as GL
import           Proofs
import           Servant
import           Servant.Auth.Server
import           Servant.Server
import           Theory.Named
import qualified UserInput
import  Control.Monad.Reader
import Config

type AppM = ReaderT Config Handler

placeStone :: UserInput.User -> Int -> AppM ()
placeStone = errPlayerExcluded

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

proposePass :: UserInput.User -> Int -> AppM ()
proposePass = errPlayerExcluded

proposeTerritory :: UserInput.User -> Int -> AppM ()
proposeTerritory = errPlayerExcluded

acceptTerritoryProposal :: UserInput.User -> Int -> AppM ()
acceptTerritoryProposal = errPlayerExcluded

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
    _ -> pure userType
