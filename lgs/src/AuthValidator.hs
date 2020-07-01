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
proposeGame (UserInput.User _ _ id) (UserInput.ProposedGame bp wp bt wt _ _) =
  do
    mUser <- GEX.getUser id
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
acceptGameProposal (UserInput.User _ _ id) gameId =
  do
    mUser <- GEX.getUser id
    case mUser of
      Nothing -> throwError err410
      Just user ->
        do
          invalidUser <- not <$> (GEX.isPlayerAwaiter (GDB._userId user) gameId)
          when invalidUser $ throwError err401

proposePass :: UserInput.User -> Int -> AppM ()
proposePass = errPlayerExcluded

proposeTerritory :: UserInput.User -> Int -> AppM ()
proposeTerritory = errPlayerExcluded

acceptTerritoryProposal :: UserInput.User -> Int -> AppM ()
acceptTerritoryProposal = errPlayerExcluded

errPlayerExcluded :: UserInput.User -> Int -> AppM ()
errPlayerExcluded (UserInput.User _ _ id) gameId =
  do
    mUser <-  GEX.getUser id
    players <- GEX.getGamePlayers gameId
    case elem <$> mUser <*> Just players of
      Nothing    -> throwError err410
      Just False -> throwError err401
      _          -> pure ()
