{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}


module GameApiImpl where

import qualified AuthValidator
import           Config
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor
import qualified Data.HashMap.Strict    as M
import           Data.List              (nub)
import           Data.Maybe
import           Data.Text              (Text)
import           Debug.Trace
import           Game                   (newGame)
import qualified Game                   as G
import           GameDB                 (awaiter_game_id, awaiter_id,
                                         awaiter_user_id, blackFocus,
                                         blackPlayer, blackTeacher, game, grId,
                                         timestamp, userEmail, userId, userName,
                                         userPasswordHash, whiteFocus)
import qualified GameDB                 as GDB
import qualified GameExpressions        as GEX
import qualified GameLogic              as GL
import qualified OutputTypes            as OT
import           Proofs
import qualified PubSub                 as PS
import qualified PubSubTypes            as PST
import           Servant
import           Theory.Named
import qualified UserInput
--TODO: REMOVE
import           Debug.Trace

type AppM = ReaderT Config Handler

placeStone :: UserInput.User -> Int -> G.Position -> AppM ((Either G.MoveError G.Outcome),G.Game)
placeStone user gameId pos = do
  lift $ AuthValidator.placeStone user gameId
  rtGmap <- asks gameMap
  name pos $ \case
    Bound pos -> do
      mGameRecord <- liftIO $ GEX.getGameRecord gameId
      mPlayerColor <- liftIO $ GEX.getPlayerColor user gameId
      let mPlaceStone = GL.placeStone <$> mPlayerColor <*> Just pos
      case (mPlaceStone, mGameRecord) of
        (Just placeStone, Just gameRecord) ->
              let ret@(_, updatedGame) =
                    runState
                      (runExceptT placeStone)
                      (gameRecord ^. game)
              in do
                trace (show "Sending place stone msg") $ liftIO . atomically $ do
                  rtGame <- PS.getGame gameId rtGmap
                  writeTChan (PST.gameChan rtGame) (PST.UpdateGame updatedGame)
                liftIO $ GEX.updateGame (const updatedGame) gameId $> ret
        (Nothing, _) -> pure (Left G.NoBoard, newGame)
        otherwise -> pure (Left G.IllegalPlayer, newGame)
    Unbound -> pure (Left G.OutOfBounds, newGame)

-- TODO: Perform validation on regex of inputs allowed
createNewUser :: UserInput.RegisterUser -> AppM ()
createNewUser (UserInput.RegisterUser email name password) = do
  mUser <- liftIO $ GEX.getUserViaName name
  when (isJust mUser) (throwError err409)
  liftIO $ GEX.insertUser email name password
  pure ()

getGameId :: Int -> AppM (Maybe OT.GameRecord)
getGameId gameId = do
  mGR <- liftIO $ GEX.getGameRecord gameId
  pure $ OT.convertGR <$> mGR


getGamesForProfile :: UserInput.User -> AppM OT.AllGames
getGamesForProfile UserInput.User{..} = getGamesForPlayer userId

getGamesForPlayer :: Int -> AppM OT.AllGames
getGamesForPlayer playerId = do
  mgrs <- liftIO $ fmap OT.convertGR <$> GEX.getPlayersGameRecords playerId
  mawts <- liftIO $ foldM (\m k -> do
                     awaiters <- fmap OT.convertAwaiter <$> GEX.getAwaiters (OT.grId k)
                     pure $ M.insert (OT.grId k) awaiters m) mempty mgrs
  pure (mgrs, mawts)

proposeGame :: UserInput.User -> UserInput.ProposedGame -> AppM OT.GameRecord
proposeGame user proposedGame@(UserInput.ProposedGame bp wp mbt mwt _ _) = do
  lift $ AuthValidator.proposeGame user proposedGame
  mPlayers <- mapM (liftIO . GEX.getUser) $ [bp, wp]
  mTeachers <- mapM (liftIO . GEX.getUser) $ catMaybes [mbt, mwt]
  let gameUsers = mPlayers ++ mTeachers
  when (nub gameUsers /= gameUsers) (throwError $
                                     err406 {errBody = "All proposed users must be unique."})
  when (Nothing `elem` mPlayers) (throwError $ err400 {errBody = "All proposed users must exist."})
  when (Nothing `elem` mTeachers) (throwError $ err400 {errBody = "All proposed teachers must exist."})
  gameRecord:_ <- liftIO $ GEX.insertGame proposedGame newGame
  mapM_ (liftIO . GEX.insertAwaiter (gameRecord ^. grId) . GDB._userId) $ catMaybes gameUsers
  pure $ OT.convertGR gameRecord

acceptGameProposal :: UserInput.User -> Int -> Bool -> AppM (Maybe G.GameStatus)
acceptGameProposal user@(UserInput.User _ name _) gameId shouldAccept = do
  lift $ AuthValidator.acceptGameProposal user gameId

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

proposePass :: UserInput.User -> Int -> AppM (Maybe G.GameStatus)
proposePass user gameId = do
  lift $ AuthValidator.proposePass user gameId
  mSpace <- liftIO $ GEX.getPlayerColor user gameId
  case mSpace of
    Nothing -> throwError err410
    Just space -> do
      mGame <- liftIO $ GEX.updateGame (GL.proposePass space) gameId
      pure ( mGame <&> (^. G.status))

proposeTerritory :: UserInput.User -> Int -> G.Territory -> AppM (Maybe G.GameStatus)
proposeTerritory user gameId territory = do
  lift $ AuthValidator.proposeTerritory user gameId
  mPlayerColor <- liftIO $ GEX.getPlayerColor user gameId
  let mProposedColor = mPlayerColor
  case mProposedColor of
    Nothing -> throwError err410
    Just proposedColor -> do
      mGame <- liftIO $ GEX.updateGame
        (GL.proposeTerritory territory proposedColor)
        gameId
      pure (mGame <&> (^. G.status))

acceptTerritoryProposal :: UserInput.User -> Int -> Bool -> AppM (Maybe G.GameStatus)
acceptTerritoryProposal user gameId shouldAccept = do
  lift $ AuthValidator.acceptTerritoryProposal user gameId
  mPlayerColor <- liftIO $ GEX.getPlayerColor user gameId
  let mUpdateState = GL.acceptTerritoryProposal <$> mPlayerColor <*> Just shouldAccept
  case mUpdateState of
    Nothing -> pure Nothing
    Just updateState -> do
      mGame <- liftIO $ GEX.updateGame updateState gameId
      pure (mGame <&> (^. G.status))
