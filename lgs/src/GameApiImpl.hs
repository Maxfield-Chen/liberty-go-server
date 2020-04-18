{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module GameApiImpl where

import qualified AuthValidator
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.List            (nub)
import           Data.Maybe
import           Data.Text            (Text)
import           Game
import           GameDB
import           GameExpressions
import qualified GameLogic            as GL
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
  when (isJust mUser) (throwError err409)
  liftIO $ insertUser email name password
  pure ()

getGameId :: Int -> Handler (Maybe GameRecord)
getGameId = liftIO . getGameRecord

getGamesForPlayer :: Int -> Handler [GameRecord]
getGamesForPlayer = liftIO . getGameRecords

proposeGame :: UserInput.User -> UserInput.ProposedGame -> Handler GameRecord
proposeGame user proposedGame@(UserInput.ProposedGame bp wp mbt mwt _ _) = do
  AuthValidator.proposeGame user proposedGame
  let gameUsers = catMaybes
                  [Just bp
                  ,Just wp
                  ,mbt
                  ,mwt]
  mUsers <- mapM (liftIO . getUser) gameUsers
  when (nub gameUsers /= gameUsers) (throwError $
                                     err406 {errBody = "All proposed users must be unique."})
  when (Nothing `elem` mUsers) (throwError $ err400 {errBody = "All proposed users must exist."})
  gameRecord:_ <- liftIO $ insertGame proposedGame newGame
  mapM_ (liftIO . insertAwaiter (_gameId gameRecord) . _userId) (catMaybes mUsers)
  pure gameRecord

acceptGameProposal :: UserInput.User -> Int -> Bool -> Handler (Maybe GameStatus)
acceptGameProposal user@(UserInput.User _ name _) gameId shouldAccept = do
  AuthValidator.acceptGameProposal user gameId

  mUser <- liftIO $ getUserViaName name
  --fromJust on mUser checked by auth validator above
  (liftIO . deleteAwaiter gameId . _userId . fromJust) mUser
  awaiters <- liftIO $ getAwaiters gameId

  --Only update proposal once all players have accepted
  case (null awaiters, shouldAccept) of
    (True, True) -> do
      mGame <- liftIO $ updateGame (GL.updateGameProposal True) gameId
      pure (mGame <&> (^. status))
    (False, True) -> pure (Just GameProposed)
    (_, False)   -> do
      --TODO: Clean up remaining awaiters here.
      mGame <- liftIO $ updateGame (GL.updateGameProposal False) gameId
      pure (mGame <&> (^. status))

proposePass :: UserInput.User -> Int -> Handler (Maybe GameStatus)
proposePass user gameId = do
  AuthValidator.proposePass user gameId
  mSpace <- liftIO $ getPlayerColor user gameId
  case mSpace of
    Nothing -> throwError err410
    Just space -> do
      mGame <- liftIO $ updateGame (GL.proposePass space) gameId
      pure ( mGame <&> (^. status))

proposeTerritory :: UserInput.User -> Int -> Territory -> Handler (Maybe GameStatus)
proposeTerritory user gameId territory = do
  AuthValidator.proposeTerritory user gameId
  mPlayerColor <- liftIO $ getPlayerColor user gameId
  let mProposedColor = mPlayerColor
  case mProposedColor of
    Nothing -> throwError err410
    Just proposedColor -> do
      mGame <- liftIO $ updateGame
        (GL.proposeTerritory territory proposedColor)
        gameId
      pure (mGame <&> (^. status))

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
