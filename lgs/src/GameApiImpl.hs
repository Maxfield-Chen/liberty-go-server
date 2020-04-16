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
import           Control.Monad.State
import           Data.Maybe
import           Data.Text            (Text)
import           Game
import           GameDB
import           GameExpressions
import qualified GameLogic            as GL
import           GHC.Generics
import           Prelude              ()
import           Prelude.Compat
import           Proofs
import           Servant
import           Theory.Named
import qualified UserInput

-- TODO: Clean this up using some monadic binds or other handling.
-- Clear triangle of doom :(
placeStone :: UserInput.User -> Int -> Position -> Handler ((Either MoveError Outcome),Game)
placeStone user gameId pos = do
  AuthValidator.placeStone user gameId
  name pos $ \case
    Bound pos -> do
      mGameRecord <- liftIO $ getGameRecord gameId
      mPlayerColor <- liftIO $ getPlayerColor user gameId
      let mPlaceStone = GL.placeStone <$> mPlayerColor <*> Just pos
      case mPlaceStone of
        Just placeStone ->
          case mGameRecord of
            Just gameRecord ->
              let ret@(_, game) =
                    runState
                      (runExceptT placeStone)
                      (_game gameRecord)
              in liftIO $ updateGame (const game) gameId >> pure ret
            Nothing -> pure (Left NoBoard, newGame)
        Nothing -> pure (Left IllegalPlayer, newGame)
    Unbound -> pure (Left OutOfBounds, newGame)

-- TODO: Perform validation on regex of inputs allowed
createNewUser :: UserInput.User -> Handler ()
createNewUser (UserInput.User email name password) = do
  mUser <- liftIO $ getUserViaName name
  when (isJust mUser) (throwError err401)
  liftIO $ insertUser email name password

getGameId :: Int -> Handler (Maybe GameRecord)
getGameId = liftIO . getGameRecord

getGamesForPlayer :: Int -> Handler [GameRecord]
getGamesForPlayer = liftIO . getGameRecords

proposeGame :: UserInput.User -> UserInput.ProposedGame -> Handler ()
proposeGame user proposedGame = do
  AuthValidator.proposeGame user proposedGame
  liftIO $ insertGame proposedGame newGame

acceptGameProposal :: UserInput.User -> Int -> Bool -> Handler (Maybe GameStatus)
acceptGameProposal user gameId shouldAccept = do
  AuthValidator.acceptGameProposal user gameId
  mGame <- liftIO $ updateGame (GL.updateGameProposal shouldAccept) gameId
  pure ( mGame <&> (^. status))

proposePass :: UserInput.User -> Int -> Space -> Handler (Maybe GameStatus)
proposePass user gameId space = do
  AuthValidator.proposePass user gameId
  mGame <- liftIO $ updateGame (GL.proposePass space) gameId
  pure ( mGame <&> (^. status))

proposeTerritory :: UserInput.User -> Int -> Territory -> Handler (Maybe GameStatus)
proposeTerritory user gameId territory = do
  AuthValidator.proposeTerritory user gameId
  mGame <- liftIO $ updateGame (GL.proposeTerritory territory) gameId
  pure ( mGame <&> (^. status))

acceptTerritoryProposal :: UserInput.User -> Int -> Bool -> Handler (Maybe GameStatus)
acceptTerritoryProposal user gameId shouldAccept = do
  AuthValidator.acceptTerritoryProposal user gameId
  mPlayerColor <- liftIO $ getPlayerColor user gameId
  let mUpdateState = GL.acceptTerritoryProposal <$> mPlayerColor <*> Just shouldAccept
  case mUpdateState of
    Nothing -> pure Nothing
    Just updateState -> do
      mGame <- liftIO $ updateGame updateState gameId
      pure (mGame <&> (^. status))
