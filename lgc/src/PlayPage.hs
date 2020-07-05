{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module PlayPage where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8)
import           Game                 (Position, boardPositions, newGame)
import qualified Game                 as G
import qualified GameLogic            as GL
import qualified OutputTypes          as OT
import qualified UserInput
import           PageUtil
import           Proofs
import           PubSubTypes          hiding (GameId, gameId)
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient        as SC
import           Text.Read            (readMaybe)
import qualified GameDB as GDB
import           Theory.Named


type GameId = Int

playPage :: forall t m . MonadWidget t m =>
            Dynamic t Page
         -> Dynamic t GameId
         -> m (Event t Page)
playPage dynPage dynGameId =
  elDynAttr "div" (shouldShow Play "play-page" <$> dynPage) $ do
    let evGetGame = updated dynGameId
        evEmptyGetGame = () <$ evGetGame
    evFetchMGR <- fmapMaybe reqSuccess <$> SC.getGame (Right  <$> dynGameId) evEmptyGetGame
    evUser <- fmapMaybe reqSuccess <$> SC.userForProfile evEmptyGetGame
    evChatMessages <- fmapMaybe reqSuccess <$> SC.getMessages (Right <$> dynGameId) evEmptyGetGame
    mGameMessage <- realTimeEl dynGameId evEmptyGetGame

    dynUser <- holdDyn OT.newUser evUser
    dynGame <- getGame dynGameId (fmap OT.grGame <$> evFetchMGR) mGameMessage
    dynGR <- holdDyn OT.newGameRecord (fromMaybe OT.newGameRecord <$> evFetchMGR)

    evPlayerPage <- playerSidebar dynGR dynUser
    boardEv       <- boardEl $ fromMaybe newGame <$> dynGame
    evOpponentPage <- opponentSidebar dynGR dynUser
    posDyn        <- holdDyn (Left "No Pos") $ Right <$> boardEv
    _ <- fmapMaybe reqSuccess <$>
      SC.placeStone (Right <$> dynGameId) posDyn (() <$ boardEv)
    pure $ leftmost [evPlayerPage, evOpponentPage]

opponentSidebar :: forall t m . MonadWidget t m =>
                 Dynamic t OT.GameRecord
              -> Dynamic t OT.User
              -> m (Event t Page)
opponentSidebar dynGameRecord dynProfileUser =
  divClass "sidebar-opponent" $ do
    evPage <- divClass "sidebar-opponent-info" $ do
      let dynOpponent = OT.getOpponent <$> dynProfileUser <*> dynGameRecord
          dynMTeacher = OT.getTeacher <$> dynOpponent <*> dynGameRecord
      evPlayer <- genDynButton "sidebar-opponent-user"
        (T.pack . show . OT.userName <$> dynOpponent)
        Profile
      evTeacher <- genDynButton "sidebar-opponent-teacher"
        (T.pack . show . OT.userName . fromMaybe OT.newUser <$> dynMTeacher)
        Profile
      pure $ leftmost [evPlayer, evTeacher]
    divClass "sidebar-opponent-chat" $ inputElement def
    pure evPage

playerSidebar :: forall t m . MonadWidget t m =>
                 Dynamic t OT.GameRecord
              -> Dynamic t OT.User
              -> m (Event t Page)
playerSidebar dynGameRecord dynProfileUser =
  divClass "sidebar-player" $ do
    evPage <- divClass "sidebar-player-info" $ do
      let dynMTeacher = OT.getTeacher <$> dynProfileUser <*> dynGameRecord
      evPlayer <- genDynButton "sidebar-player-user"
        (T.pack . show . OT.userName <$> dynProfileUser)
        Profile
      evTeacher <- genDynButton "sidebar-player-teacher"
        (T.pack . show . OT.userName . fromMaybe OT.newUser <$> dynMTeacher)
        Profile
      pure $ leftmost [evPlayer, evTeacher]
    divClass "sidebar-player-chat" $ inputElement def
    pure evPage

boardEl :: forall t m . MonadWidget t m =>
           Dynamic t G.Game
           -> m (Event t Position)
boardEl dynGame =
  divClass "board-container" $ do
    divClass "board-top" $ text ""
    divClass "board-left" $ text ""
    evPos <- divClass "board-overlay" $ divClass "board-grid" $ do
      buttonEvs <- foldr (\pos mButtonEvs -> name pos $
                                    \case
                                        Bound boundPos -> do
                                          let dynSpace = (GL.getPosition boundPos) <$> dynGame
                                          buttonEv <- boardButton pos dynSpace
                                          (:) buttonEv <$> mButtonEvs
                                        _ -> error "unbound position when creating boardEl")
                                  (pure [] :: m [Event t Position])
                                  (concat boardPositions)
      pure $ leftmost buttonEvs
    divClass "board-right" $ text ""
    divClass "board-bottom" $ text ""
    pure evPos

getGame :: forall t m. MonadWidget t m =>
             Dynamic t GameId
          -> Event t (Maybe G.Game)
          -> Event t (Maybe GameMessage)
          -> m (Dynamic t (Maybe G.Game))
getGame dynGameId evMFetchGame mGameMessage = do
  dynMGameMessage <- holdDyn (Just New) mGameMessage
  let dynMWSGame = getGameFromUpdate <$> dynGameId <*> dynMGameMessage
  dynMFetchGame <- holdDyn (Just newGame) evMFetchGame
  pure $ (\mws mhttp -> case mws of
                       Just ws -> Just ws
                       Nothing -> mhttp) <$> dynMFetchGame <*> dynMWSGame

getGameFromUpdate :: Int -> Maybe GameMessage -> Maybe G.Game
getGameFromUpdate gameId = (=<<) (\case
                              UpdateGame (OT.GameUpdate gid g) -> case gid == gameId of
                                True  -> Just g
                                False -> Nothing
                              _            -> Nothing)

--TODO: Determine when to close WS connections, since they are getting overwhelming.
-- Alternatively, see if one connection can be shared throughout the program.
realTimeEl :: forall t m. MonadWidget t m =>
              Dynamic t GameId
           -> Event t ()
           -> m (Event t (Maybe GameMessage))
realTimeEl dynGameId b = do
  let joinMessage = (encodeUtf8 . (<> "}") . ("{\"type\": \"join\",\"gameId\": " <>)
                      . T.pack . show )
                      <$> tagPromptlyDyn dynGameId b
      newMessage = (:[]) <$> joinMessage
  evMGameMessage :: Event t (Maybe GameMessage) <- do
    ws <- webSocket "ws://localhost:8888" $ def &
      webSocketConfig_send .~ newMessage
    pure $ decode <$> BL.fromStrict <$> _webSocket_recv ws
  pure evMGameMessage

-- getMessages :: forall t m. MonadWidget t m =>
--                Dynamic t GameId
--                -> Event
chatEl :: forall t m. MonadWidget t m =>
          Dynamic t GameId
          -> Dynamic t [GDB.ChatMessage]
          -> Bool
          -> m ()
chatEl dynGameId dynChatMessages shared = divClass "chat" $ do
  divClass "chat-messages" $ do
    simpleList dynChatMessages
      (\dynMessage -> divClass "chat-message" $ dynText $ GDB._chat_message_content <$> dynMessage)
  divClass "chat-send-bar" $ do
    sendInput <- textInput def
    let dynValue = _textInput_value sendInput
        dynSendInput = flip UserInput.ChatMessage shared <$> dynValue
        evSendMessage = textInputGetEnter sendInput
    fmapMaybe reqSuccess <$> SC.sendMessage (Right <$> dynGameId) ( Right <$> dynSendInput) evSendMessage
  pure ()
