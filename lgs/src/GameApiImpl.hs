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

module GameApiImpl where

import qualified AuthValidator
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                  (Text)
import           Game
import           GameDB                     hiding (User)
import           GameExpressions
import qualified GameLogic                  as GL
import           GHC.Generics
import           Prelude                    ()
import           Prelude.Compat
import           Proofs
import           Servant
import           Servant.Auth.Server
import           Theory.Named
import qualified UserInput



-- TODO: find a more graceful return type when unbound or game not found
placeStone :: Int -> Position -> Handler ((Either MoveError Outcome),Game)
placeStone gameId pos =
  name pos $ \case
    Bound pos -> do
      mGameRecord <- liftIO $ getGameRecord gameId
      case mGameRecord of
        Just gameRecord ->
          let ret@(_, game) =
                runState
                  (runExceptT (GL.placeStone pos))
                  (_game gameRecord)
           in liftIO $ updateGame (const game) gameId >> pure ret
        Nothing -> pure (Left NoBoard, newGame)
    Unbound -> pure (Left OutOfBounds, newGame)

createNewUser :: UserInput.User -> Handler ()
createNewUser user = liftIO $ insertUser (UserInput.userEmail user) (UserInput.userName user) (UserInput.userPassword user)

getGameId :: Int -> Handler (Maybe GameRecord)
getGameId = liftIO . getGameRecord

getGamesForPlayer :: Int -> Handler [GameRecord]
getGamesForPlayer = liftIO . getGameRecords

proposeGame :: UserInput.User -> UserInput.ProposedGame -> Handler ()
proposeGame user proposedGame =
  do
    AuthValidator.proposeGame user proposedGame
    liftIO $ insertGame proposedGame newGame

acceptGameProposal :: UserInput.User -> Int -> Bool -> Handler (Maybe GameStatus)
acceptGameProposal user gameId shouldAccept = do
  -- AuthValidator.acceptGameProposal user gameId
  liftIO $ updateGameProposal gameId shouldAccept

updatePassProposal :: Int -> Space -> Handler (Maybe GameStatus)
updatePassProposal gameId space = do
  mGame <- liftIO $ updateGame (GL.proposePass space) gameId
  case mGame of
    Just game -> pure (Just (game ^. status))
    Nothing   -> pure Nothing

proposeTerritory :: Int -> Territory -> Handler (Maybe GameStatus)
proposeTerritory gameId territory = do
  mGame <- liftIO $ updateGame (GL.proposeTerritory territory) gameId
  case mGame of
    Just game -> pure (Just (game ^. status))
    Nothing   -> pure Nothing

acceptTerritoryProposal :: Int -> Bool -> Handler (Maybe GameStatus)
acceptTerritoryProposal gameId shouldAccept =
  liftIO $ updateTerritoryProposal gameId shouldAccept
