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
import qualified Data.Time              as Time
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
  config <- ask
  rtGmap <- asks gameMap
  name pos $ \case
    Bound pos -> do
      mGameRecord <-  liftIO $ runReaderT (GEX.getGameRecord gameId) config
      mPlayerColor <- liftIO $ runReaderT (GEX.getPlayerColor user gameId) config
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
                liftIO $ runReaderT (GEX.updateGame (const updatedGame) gameId ) config $> ret
        (Nothing, _) -> pure (Left G.NoBoard, newGame)
        otherwise -> pure (Left G.IllegalPlayer, newGame)
    Unbound -> pure (Left G.OutOfBounds, newGame)

-- TODO: Perform validation on regex of inputs allowed
createNewUser :: UserInput.RegisterUser -> AppM ()
createNewUser (UserInput.RegisterUser email name image password) = do
  config <- ask
  mUser <-  liftIO $ runReaderT (GEX.getUserViaName name) config
  when (isJust mUser) (throwError err409)
  when (image <= 0 || image >= length [minBound :: GDB.ProfileImage ..maxBound]) (throwError err401)
  liftIO $ runReaderT (GEX.insertUser email name image password) config
  pure ()

getUserId :: Int -> AppM (Maybe OT.User)
getUserId userId = do
  config <- ask
  maybeUser <-  liftIO $ runReaderT (GEX.getUser userId) config
  case maybeUser of
    Nothing   -> pure Nothing
    Just user -> pure . Just $ OT.convertUser user

getGameId :: Int -> AppM (Maybe OT.GameRecord)
getGameId gameId = do
  config <- ask
  mGR <-  liftIO $ runReaderT (GEX.getGameRecord gameId) config
  case mGR of
    Nothing -> pure Nothing
    Just gr -> liftIO $ runReaderT (GEX.convertDeepGameRecord gr) config

getMarkedMoves :: UserInput.User -> Int -> AppM [OT.MarkedMove]
getMarkedMoves UserInput.User{..} gameId = do
  config <- ask
  liftIO $ fmap OT.convertMarkedMove <$> runReaderT (GEX.getMarkedMoves gameId) config

getGamesForProfile :: UserInput.User -> AppM OT.AllGames
getGamesForProfile UserInput.User{..} = getGamesForPlayer userId

getUser :: UserInput.User -> AppM OT.User
getUser UserInput.User{..} = pure $ OT.User userId userName userImage userEmail

getGamesForPlayer :: Int -> AppM OT.AllGames
getGamesForPlayer playerId = do
  config <- ask
  mGDBgrs <-  liftIO $ runReaderT (GEX.getPlayersGameRecords playerId) config
  mgrs <-  mapM
    (\gameRecord -> liftIO $ runReaderT (GEX.convertDeepGameRecord gameRecord) config)
    mGDBgrs
  let grs = catMaybes mgrs
  mawts <- foldM (\m k -> do
                     awaiters <- liftIO $ fmap OT.convertAwaiter <$> runReaderT (GEX.getAwaiters (OT.grId k)) config
                     pure $ M.insert (OT.grId k) awaiters m) mempty grs
  pure (grs, mawts)

proposeGame :: UserInput.User -> UserInput.ProposedGame -> AppM OT.GameRecord
proposeGame proposingUser proposedGame@(UserInput.ProposedGame bp wp mbt mwt _ _) = do
  AuthValidator.proposeGame proposingUser proposedGame
  config <- ask
  mPlayers <- mapM (\userId -> liftIO $ runReaderT (GEX.getUser userId) config) $ [bp, wp]
  mTeachers <- mapM (\userId -> liftIO $ runReaderT (GEX.getUser userId) config) $ catMaybes [mbt, mwt]
  let gameUsers = mPlayers ++ mTeachers
  when (nub gameUsers /= gameUsers) (throwError $
                                     err406 {errBody = "All proposed users must be unique."})
  when (Nothing `elem` mPlayers) (throwError $ err400 {errBody = "All proposed users must exist."})
  when (Nothing `elem` mTeachers) (throwError $ err400 {errBody = "All proposed teachers must exist."})
  gameRecord:_ <- liftIO $ runReaderT (GEX.insertGame proposedGame newGame) config
  mapM_ (\userId -> liftIO $ runReaderT (GEX.insertAwaiter (gameRecord ^. grId) userId) config) $
    delete (UserInput.userId proposingUser) $ GDB._userId <$> catMaybes gameUsers
  mGR <-  liftIO $ runReaderT (GEX.convertDeepGameRecord gameRecord) config
  pure $ fromJust mGR

markMove :: UserInput.User -> Int -> UserInput.MarkedMove -> AppM [OT.MarkedMove]
markMove user@(UserInput.User _ _ _ userId) gameId UserInput.MarkedMove{..} = do
  mGameRecord <- AuthValidator.markMove user gameId
  let turnNumber = ((+ (-1)) . length . G._record . OT.grGame . fromJust ) mGameRecord
  config <- ask
  baseMove <-
    liftIO $ runReaderT (GEX.insertMarkedMove gameId userId turnNumber) config
  pure $ OT.convertMarkedMove <$> baseMove

sendMessage :: UserInput.User -> Int ->  UserInput.ChatMessage -> AppM ()
sendMessage user@(UserInput.User _ _ _ userId) gameId (UserInput.ChatMessage message shared) = do
  userType <- AuthValidator.sendMessage user gameId shared
  realTimeGameMap <- asks gameMap
  timestamp <- Time.zonedTimeToLocalTime <$> liftIO Time.getZonedTime
  let outputMessage = OT.ChatMessage userId message gameId userType shared timestamp
  liftIO . atomically $ do
    realTimeGame <- PS.getGame gameId realTimeGameMap
    writeTChan (PST.gameChan realTimeGame) (PST.ChatMessage outputMessage)
  config <- ask
  liftIO $ runReaderT (GEX.insertChatMessage userId message shared gameId) config
  pure ()

getMessages :: UserInput.User -> Int -> AppM [OT.ChatMessage]
getMessages (UserInput.User _ _ _ userId) gameId = do
  config <- ask
  messages <- liftIO $ runReaderT (GEX.getMessages gameId) config
  userType <- liftIO $ runReaderT (GEX.getUserType userId gameId) config
  mGameRecord <- liftIO $ runReaderT (GEX.getGameRecord gameId) config
  let gameInProgress = fromMaybe True $ (/= G.TerritoryAccepted) . G._status . GDB._game <$> mGameRecord
  pure $ fmap OT.convertChatMessage $ filter ( shouldShowMessages userType gameInProgress) messages

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
acceptGameProposal user@(UserInput.User _ name _ _) gameId shouldAccept = do
  AuthValidator.acceptGameProposal user gameId

  config <- ask
  mUser <- liftIO $ runReaderT (GEX.getUserViaName name) config
  --fromJust on mUser checked by auth validator above
  liftIO $ runReaderT ((GEX.deleteAwaiter gameId . GDB._userId . fromJust) mUser) config
  awaiters <- liftIO $ runReaderT (GEX.getAwaiters gameId) config

  --Only update proposal once all players have accepted
  case (null awaiters, shouldAccept) of
    (True, True) -> do
      mGame <-liftIO $ runReaderT (GEX.updateGame (GL.updateGameProposal True) gameId) config
      pure (mGame <&> (^. G.status))
    (False, True) -> pure (Just G.GameProposed)
    (_, False)   -> do
      --TODO: Clean up remaining awaiters here.
      mGame <- liftIO $ runReaderT (GEX.updateGame (GL.updateGameProposal False) gameId) config
      pure (mGame <&> (^. G.status))

proposePass :: UserInput.User -> Int -> AppM (Maybe G.GameStatus)
proposePass user gameId = do
  AuthValidator.proposePass user gameId
  config <- ask
  mSpace <- liftIO $ runReaderT (GEX.getPlayerColor user gameId) config
  case mSpace of
    Nothing -> throwError err410
    Just space -> do
      mGame <-liftIO $ runReaderT (GEX.updateGame (GL.proposePass space) gameId) config
      pure ( mGame <&> (^. G.status))

proposeTerritory :: UserInput.User -> Int -> G.Territory -> AppM (Maybe G.GameStatus)
proposeTerritory user gameId territory = do
  AuthValidator.proposeTerritory user gameId
  config <- ask
  mPlayerColor <- liftIO $ runReaderT (GEX.getPlayerColor user gameId) config
  let mProposedColor = mPlayerColor
  case mProposedColor of
    Nothing -> throwError err410
    Just proposedColor -> do
      mGame <- liftIO $ runReaderT (GEX.updateGame (GL.proposeTerritory territory proposedColor) gameId) config
      pure (mGame <&> (^. G.status))

acceptTerritoryProposal :: UserInput.User -> Int -> Bool -> AppM (Maybe G.GameStatus)
acceptTerritoryProposal user gameId shouldAccept = do
  AuthValidator.acceptTerritoryProposal user gameId
  config <- ask
  mPlayerColor <- liftIO $ runReaderT (GEX.getPlayerColor user gameId) config
  let mUpdateState = GL.acceptTerritoryProposal <$> mPlayerColor <*> Just shouldAccept
  case mUpdateState of
    Nothing -> pure Nothing
    Just updateState -> do
      mGame <- liftIO $ runReaderT (GEX.updateGame updateState gameId) config
      pure (mGame <&> (^. G.status))
