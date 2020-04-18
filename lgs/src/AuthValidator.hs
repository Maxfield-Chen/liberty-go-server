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


placeStone :: UserInput.User -> Int -> Handler ()
placeStone = errPlayerExcluded

proposeGame :: UserInput.User -> UserInput.ProposedGame -> Handler ()
proposeGame (UserInput.User _ name _) (UserInput.ProposedGame bp wp bt wt _ _) =
  do
    mUser <- liftIO $ GEX.getUserViaName name
    case mUser of
      Nothing -> throwError err410
      Just user -> do
        let callingUserIncluded =
              foldr
                    (\uid t -> t || uid == GDB._userId user)
                    False
                    [bp, wp, fromMaybe (-1) bt,  fromMaybe (-1) wt]
        when (not callingUserIncluded) $ throwError err401

acceptGameProposal :: UserInput.User -> Int -> Handler ()
acceptGameProposal (UserInput.User _ name _) gameId =
  do
    mUser <- liftIO $ GEX.getUserViaName name
    case mUser of
      Nothing -> throwError err410
      Just user ->
        do
          invalidUser <- not <$> liftIO  (GEX.isPlayerAwaiter (GDB._userId user) gameId)
          when invalidUser $ throwError err401

proposePass :: UserInput.User -> Int -> Handler ()
proposePass = errPlayerExcluded

proposeTerritory :: UserInput.User -> Int -> Handler ()
proposeTerritory = errPlayerExcluded

acceptTerritoryProposal :: UserInput.User -> Int -> Handler ()
acceptTerritoryProposal = errPlayerExcluded

errPlayerExcluded :: UserInput.User -> Int -> Handler ()
errPlayerExcluded (UserInput.User _ name _) gameId =
  do
    mUser <- liftIO $ GEX.getUserViaName name
    players <- liftIO $ GEX.getGamePlayers gameId
    case elem <$> mUser <*> Just players of
      Nothing    -> throwError err410
      Just False -> throwError err401
      _          -> pure ()
