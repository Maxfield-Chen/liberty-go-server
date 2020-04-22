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
import           Data.Functor
import           Data.List            (nub)
import           Data.Maybe
import           Data.Text            (Text)
import           Game                 (newGame)
import qualified Game                 as G
import           GameDB               (awaiter_game_id, awaiter_id,
                                       awaiter_user_id, blackFocus, blackPlayer,
                                       blackTeacher, game, grId, timestamp,
                                       userEmail, userId, userName,
                                       userPasswordHash, whiteFocus)
import qualified GameDB               as GDB
import qualified GameExpressions      as GEX
import qualified GameLogic            as GL
import           Proofs
import           Servant
import           Theory.Named
import qualified UserInput

-- TODO: Clean this up using some monadic binds or other handling.
-- Clear triangle of doom :(
placeStone :: UserInput.User -> Int -> G.Position -> Handler ((Either G.MoveError G.Outcome),G.Game)
placeStone user gameId pos = do
  AuthValidator.placeStone user gameId
  name pos $ \case
    Bound pos -> do
      mGameRecord <- liftIO $ GEX.getGameRecord gameId
      mPlayerColor <- liftIO $ GEX.getPlayerColor user gameId
      let mPlaceStone = GL.placeStone <$> mPlayerColor <*> Just pos
      case mPlaceStone of
        Just placeStone ->
          case mGameRecord of
            Just gameRecord ->
              let ret@(_, updatedGame) =
                    runState
                      (runExceptT placeStone)
                      (gameRecord ^. game)
              in liftIO $ GEX.updateGame (const updatedGame) gameId $> ret
            Nothing -> pure (Left G.NoBoard, newGame)
        Nothing -> pure (Left G.IllegalPlayer, newGame)
    Unbound -> pure (Left G.OutOfBounds, newGame)

-- TODO: Perform validation on regex of inputs allowed
createNewUser :: UserInput.User -> Handler ()
createNewUser (UserInput.User email name password) = do
  mUser <- liftIO $ GEX.getUserViaName name
  when (isJust mUser) (throwError err409)
  liftIO $ GEX.insertUser email name password
  pure ()

getGameId :: Int -> Handler (Maybe GDB.GameRecord)
getGameId = liftIO . GEX.getGameRecord

getGamesForPlayer :: Int -> Handler [GDB.GameRecord]
getGamesForPlayer = liftIO . GEX.getGameRecords

proposeGame :: UserInput.User -> UserInput.ProposedGame -> Handler GDB.GameRecord
proposeGame user proposedGame@(UserInput.ProposedGame bp wp mbt mwt _ _) = do
  AuthValidator.proposeGame user proposedGame
  let gameUsers = catMaybes
                  [Just bp
                  ,Just wp
                  ,mbt
                  ,mwt]
  mUsers <- mapM (liftIO . GEX.getUser) gameUsers
  when (nub gameUsers /= gameUsers) (throwError $
                                     err406 {errBody = "All proposed users must be unique."})
  when (Nothing `elem` mUsers) (throwError $ err400 {errBody = "All proposed users must exist."})
  gameRecord:_ <- liftIO $ GEX.insertGame proposedGame newGame
  mapM_ (liftIO . GEX.insertAwaiter (gameRecord ^. grId) . GDB._userId) (catMaybes mUsers)
  pure gameRecord

acceptGameProposal :: UserInput.User -> Int -> Bool -> Handler (Maybe G.GameStatus)
acceptGameProposal user@(UserInput.User _ name _) gameId shouldAccept = do
  AuthValidator.acceptGameProposal user gameId

  mUser <- liftIO $ GEX.getUserViaName name
  --fromJust on mUser checked by auth validator above
  (liftIO . GEX.deleteAwaiter gameId . GDB._userId . fromJust) mUser
  awaiters <- liftIO $ GEX.getAwaiters gameId

  --Only update proposal once all players have accepted
  case (null awaiters, shouldAccept) of
    (True, True) -> do
      mGame <- liftIO $ GEX.updateGame (GL.updateGameProposal True) gameId
      pure (mGame <&> (^. G.status))
    (False, True) -> pure (Just G.GameProposed)
    (_, False)   -> do
      --TODO: Clean up remaining awaiters here.
      mGame <- liftIO $ GEX.updateGame (GL.updateGameProposal False) gameId
      pure (mGame <&> (^. G.status))

proposePass :: UserInput.User -> Int -> Handler (Maybe G.GameStatus)
proposePass user gameId = do
  AuthValidator.proposePass user gameId
  mSpace <- liftIO $ GEX.getPlayerColor user gameId
  case mSpace of
    Nothing -> throwError err410
    Just space -> do
      mGame <- liftIO $ GEX.updateGame (GL.proposePass space) gameId
      pure ( mGame <&> (^. G.status))

proposeTerritory :: UserInput.User -> Int -> G.Territory -> Handler (Maybe G.GameStatus)
proposeTerritory user gameId territory = do
  AuthValidator.proposeTerritory user gameId
  mPlayerColor <- liftIO $ GEX.getPlayerColor user gameId
  let mProposedColor = mPlayerColor
  case mProposedColor of
    Nothing -> throwError err410
    Just proposedColor -> do
      mGame <- liftIO $ GEX.updateGame
        (GL.proposeTerritory territory proposedColor)
        gameId
      pure (mGame <&> (^. G.status))

acceptTerritoryProposal :: UserInput.User -> Int -> Bool -> Handler (Maybe G.GameStatus)
acceptTerritoryProposal user gameId shouldAccept = do
  AuthValidator.acceptTerritoryProposal user gameId
  mPlayerColor <- liftIO $ GEX.getPlayerColor user gameId
  let mUpdateState = GL.acceptTerritoryProposal <$> mPlayerColor <*> Just shouldAccept
  case mUpdateState of
    Nothing -> pure Nothing
    Just updateState -> do
      mGame <- liftIO $ GEX.updateGame updateState gameId
      pure (mGame <&> (^. G.status))
