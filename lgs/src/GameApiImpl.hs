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
import           Data.List              (delete, nub)
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
  AuthValidator.placeStone user gameId
  rtGmap <- asks gameMap
  name pos $ \case
    Bound pos -> do
      mGameRecord <-  GEX.getGameRecord gameId
      mPlayerColor <- GEX.getPlayerColor user gameId
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
                  writeTChan (PST.gameChan rtGame) (PST.UpdateGame $ OT.GameUpdate gameId updatedGame)
                GEX.updateGame (const updatedGame) gameId $> ret
        (Nothing, _) -> pure (Left G.NoBoard, newGame)
        otherwise -> pure (Left G.IllegalPlayer, newGame)
    Unbound -> pure (Left G.OutOfBounds, newGame)

-- TODO: Perform validation on regex of inputs allowed
createNewUser :: UserInput.RegisterUser -> AppM ()
createNewUser (UserInput.RegisterUser email name password) = do
  mUser <-  GEX.getUserViaName name
  when (isJust mUser) (throwError err409)
  GEX.insertUser email name password
  pure ()

getGameId :: Int -> AppM (Maybe OT.GameRecord)
getGameId gameId = do
  mGR <-  GEX.getGameRecord gameId
  case mGR of
    Nothing -> pure Nothing
    Just gr -> convertGR gr

--TODO: Validate that this doesn't return Nothing when teachers are null
convertGR :: GDB.GameRecord ->  AppM (Maybe OT.GameRecord)
convertGR gr = do
    let GDB.UserId bpId = GDB._black_player gr
        GDB.UserId wpId = GDB._white_player gr
        GDB.UserId mbtId = GDB._black_teacher gr
        GDB.UserId mwtId = GDB._white_teacher gr
    mbp <- GEX.getUser bpId
    mwp <- GEX.getUser wpId
    mbt <- case mbtId of
      Nothing   -> pure Nothing
      Just btId -> GEX.getUser btId
    mwt <- case mwtId of
      Nothing   -> pure Nothing
      Just wtId -> GEX.getUser wtId
    pure $ ((OT.convertGR gr) <$> mbp <*> mwp) <*> Just mbt <*> Just mwt


getGamesForProfile :: UserInput.User -> AppM OT.AllGames
getGamesForProfile UserInput.User{..} = getGamesForPlayer userId

getUser :: UserInput.User -> AppM OT.User
getUser UserInput.User{..} = pure $ OT.User userId userName userEmail

getGamesForPlayer :: Int -> AppM OT.AllGames
getGamesForPlayer playerId = do
  mGDBgrs <-  GEX.getPlayersGameRecords playerId
  mgrs <-  mapM convertGR mGDBgrs
  let grs = catMaybes mgrs
  mawts <- foldM (\m k -> do
                     awaiters <- fmap OT.convertAwaiter <$> GEX.getAwaiters (OT.grId k)
                     pure $ M.insert (OT.grId k) awaiters m) mempty grs
  pure (grs, mawts)

proposeGame :: UserInput.User -> UserInput.ProposedGame -> AppM OT.GameRecord
proposeGame proposingUser proposedGame@(UserInput.ProposedGame bp wp mbt mwt _ _) = do
  AuthValidator.proposeGame proposingUser proposedGame
  mPlayers <- mapM (GEX.getUser) $ [bp, wp]
  mTeachers <- mapM (GEX.getUser) $ catMaybes [mbt, mwt]
  let gameUsers = mPlayers ++ mTeachers
  when (nub gameUsers /= gameUsers) (throwError $
                                     err406 {errBody = "All proposed users must be unique."})
  when (Nothing `elem` mPlayers) (throwError $ err400 {errBody = "All proposed users must exist."})
  when (Nothing `elem` mTeachers) (throwError $ err400 {errBody = "All proposed teachers must exist."})
  gameRecord:_ <- GEX.insertGame proposedGame newGame
  mapM_ (GEX.insertAwaiter (gameRecord ^. grId)) $
    delete (UserInput.userId proposingUser) $ GDB._userId <$> catMaybes gameUsers
  mGR <-  convertGR gameRecord
  pure $ fromJust mGR

sendMessage :: UserInput.User -> Int ->  UserInput.ChatMessage -> AppM [GDB.ChatMessage]
sendMessage user@(UserInput.User _ _ userId) gameId (UserInput.ChatMessage message shared) = do
  AuthValidator.sendMessage user gameId shared
  realTimeGameMap <- asks gameMap
  liftIO . atomically $ do
    realTimeGame <- PS.getGame gameId realTimeGameMap
    writeTChan (PST.gameChan realTimeGame) (PST.ChatMessage $ OT.ChatMessage userId message gameId shared)
  GEX.insertChatMessage userId message shared gameId

getMessages :: UserInput.User -> Int -> AppM [GDB.ChatMessage]
getMessages (UserInput.User _ _ userId) gameId = do
   messages <- GEX.getMessages gameId
   userType <- GEX.getUserType userId gameId
   mGameRecord <- GEX.getGameRecord gameId
   let gameInProgress = fromMaybe True $ (==) G.InProgress . G._status . GDB._game <$> mGameRecord
   pure $ filter ( shouldShowMessages userType gameInProgress) messages

shouldShowMessages :: GDB.UserType -> Bool -> GDB.ChatMessage->  Bool
shouldShowMessages userType gameInProgress message =
   case (userType,gameInProgress) of
      (_, False) -> True
      (GDB.Watcher, True) -> messageIsShared message
      (GDB.BlackPlayer,True) ->
        (messageIsShared message ||
          message ^. GDB.chat_message_user_type == GDB.BlackPlayer ||
          message ^. GDB.chat_message_user_type == GDB.BlackTeacher)
      (GDB.BlackTeacher,True) ->
        (messageIsShared message ||
          message ^. GDB.chat_message_user_type == GDB.BlackPlayer ||
          message ^. GDB.chat_message_user_type == GDB.BlackTeacher)
      (GDB.WhitePlayer,True) ->
        (messageIsShared message ||
          message ^. GDB.chat_message_user_type == GDB.WhitePlayer ||
          message ^. GDB.chat_message_user_type == GDB.WhiteTeacher)
      (GDB.WhiteTeacher,True) ->
        (messageIsShared message ||
          message ^. GDB.chat_message_user_type == GDB.WhitePlayer ||
          message ^. GDB.chat_message_user_type == GDB.WhiteTeacher)


messageIsShared :: GDB.ChatMessage -> Bool
messageIsShared = ((==) True . GDB._chat_message_shared)

acceptGameProposal :: UserInput.User -> Int -> Bool -> AppM (Maybe G.GameStatus)
acceptGameProposal user@(UserInput.User _ name _) gameId shouldAccept = do
  AuthValidator.acceptGameProposal user gameId

  mUser <- GEX.getUserViaName name
  --fromJust on mUser checked by auth validator above
  (GEX.deleteAwaiter gameId . GDB._userId . fromJust) mUser
  awaiters <- GEX.getAwaiters gameId

  --Only update proposal once all players have accepted
  case (null awaiters, shouldAccept) of
    (True, True) -> do
      mGame <-GEX.updateGame (GL.updateGameProposal True) gameId
      pure (mGame <&> (^. G.status))
    (False, True) -> pure (Just G.GameProposed)
    (_, False)   -> do
      --TODO: Clean up remaining awaiters here.
      mGame <- GEX.updateGame (GL.updateGameProposal False) gameId
      pure (mGame <&> (^. G.status))

proposePass :: UserInput.User -> Int -> AppM (Maybe G.GameStatus)
proposePass user gameId = do
  AuthValidator.proposePass user gameId
  mSpace <- GEX.getPlayerColor user gameId
  case mSpace of
    Nothing -> throwError err410
    Just space -> do
      mGame <-GEX.updateGame (GL.proposePass space) gameId
      pure ( mGame <&> (^. G.status))

proposeTerritory :: UserInput.User -> Int -> G.Territory -> AppM (Maybe G.GameStatus)
proposeTerritory user gameId territory = do
  AuthValidator.proposeTerritory user gameId
  mPlayerColor <- GEX.getPlayerColor user gameId
  let mProposedColor = mPlayerColor
  case mProposedColor of
    Nothing -> throwError err410
    Just proposedColor -> do
      mGame <- GEX.updateGame
        (GL.proposeTerritory territory proposedColor)
        gameId
      pure (mGame <&> (^. G.status))

acceptTerritoryProposal :: UserInput.User -> Int -> Bool -> AppM (Maybe G.GameStatus)
acceptTerritoryProposal user gameId shouldAccept = do
  AuthValidator.acceptTerritoryProposal user gameId
  mPlayerColor <- GEX.getPlayerColor user gameId
  let mUpdateState = GL.acceptTerritoryProposal <$> mPlayerColor <*> Just shouldAccept
  case mUpdateState of
    Nothing -> pure Nothing
    Just updateState -> do
      mGame <- GEX.updateGame updateState gameId
      pure (mGame <&> (^. G.status))
