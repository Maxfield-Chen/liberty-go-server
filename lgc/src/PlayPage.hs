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
import           GameDB               (UserType (..))
import qualified GameDB               as GDB
import qualified GameLogic            as GL
import qualified OutputTypes          as OT
import           PageUtil
import           Proofs
import           PubSubTypes          hiding (GameId, gameId)
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient        as SC
import           Text.Read            (readMaybe)
import           Theory.Named
import qualified UserInput


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
    dynChatMessages <- getChatMessages dynGameId evChatMessages mGameMessage

    evPlayerPage <- playerSidebar dynGR dynChatMessages dynUser
    boardEv       <- boardEl $ fromMaybe newGame <$> dynGame
    evOpponentPage <- opponentSidebar dynGR dynChatMessages dynUser
    posDyn        <- holdDyn (Left "No Pos") $ Right <$> boardEv
    _ <- fmapMaybe reqSuccess <$>
      SC.placeStone (Right <$> dynGameId) posDyn (() <$ boardEv)
    pure $ leftmost [evPlayerPage, evOpponentPage]

opponentSidebar :: forall t m . MonadWidget t m =>
                 Dynamic t OT.GameRecord
              -> Dynamic t [ OT.ChatMessage]
              -> Dynamic t OT.User
              -> m (Event t Page)
opponentSidebar dynGameRecord dynChatMessages dynProfileUser =
  divClass "sidebar-opponent" $ do
    evPage <- divClass "sidebar-opponent-info" $ do
      let dynOpponent = OT.getOpponent <$> dynProfileUser <*> dynGameRecord
          dynMTeacher = OT.getTeacher <$> dynOpponent <*> dynGameRecord
      evTeacher <- dynButton
        (T.pack . show . (\image -> toEnum image :: GDB.ProfileImage) . OT.userImage . fromMaybe OT.newUser <$> dynMTeacher)
        "sidebar-opponent-teacher"
        Profile
      evPlayer <- dynButton
        (T.pack . show . (\image -> toEnum image :: GDB.ProfileImage) . OT.userImage <$> dynOpponent)
        "sidebar-opponent-user"
        Profile
      pure $ leftmost [evPlayer, evTeacher]
    divClass "sidebar-player-chat" $ chatEl dynGameRecord dynChatMessages dynProfileUser True rightFilter
    pure evPage

playerSidebar :: forall t m . MonadWidget t m =>
                 Dynamic t OT.GameRecord
              -> Dynamic t [ OT.ChatMessage]
              -> Dynamic t OT.User
              -> m (Event t Page)
playerSidebar dynGameRecord dynChatMessages dynProfileUser =
  divClass "sidebar-player" $ do
    evPage <- divClass "sidebar-player-info" $ do
      let dynMTeacher = OT.getTeacher <$> dynProfileUser <*> dynGameRecord
      evPlayer <- dynButton
        (T.pack . show . (\image -> toEnum image :: GDB.ProfileImage) . OT.userImage <$> dynProfileUser)
        "sidebar-player-user"
        Profile
      evTeacher <- dynButton
        (T.pack . show . (\image -> toEnum image :: GDB.ProfileImage) . OT.userImage . fromMaybe OT.newUser <$> dynMTeacher)
        "sidebar-player-teacher"
        Profile
      pure $ leftmost [evPlayer, evTeacher]
    divClass "sidebar-player-chat" $ chatEl dynGameRecord dynChatMessages dynProfileUser False leftFilter
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
                       Nothing -> mhttp) <$> dynMWSGame <*> dynMFetchGame

getChatMessages:: forall t m. MonadWidget t m =>
                  Dynamic t GameId
                  -> Event t [OT.ChatMessage]
                  -> Event t (Maybe GameMessage)
                  -> m (Dynamic t [OT.ChatMessage])
getChatMessages dynGameId evFetchMessages evMMessages = do
  dynMMessages <- foldDyn (\new messages -> messages <> [new]) [] evMMessages
  let dynRealTimeMessage = (\messages gameId -> map (getChatMessageFromUpdate gameId) messages) <$> dynMMessages <*> dynGameId

  dynFetchMessages <- holdDyn [] evFetchMessages
  pure $ (\messages new -> messages <> catMaybes new) <$> dynFetchMessages <*> dynRealTimeMessage

getChatMessageFromUpdate :: Int -> Maybe GameMessage -> Maybe OT.ChatMessage
getChatMessageFromUpdate  gameId = (=<<) (\case
                                             ChatMessage outputMessage ->
                                               case gameId == OT.chatMessageGameId outputMessage of
                                                True  -> Just outputMessage
                                                False -> Nothing
                                             _ -> Nothing)



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

chatEl :: forall t m. MonadWidget t m =>
          Dynamic t OT.GameRecord
          -> Dynamic t [OT.ChatMessage]
          -> Dynamic t OT.User
          -> Bool
          -> ([OT.ChatMessage] -> Bool -> [OT.ChatMessage])
          -> m ()
chatEl dynGameRecord dynChatMessages dynProfileUser shared filterMessages = divClass "chat" $ do
  let dynIsBlack = OT.isBlack <$> dynProfileUser <*> dynGameRecord
      dynGameId = OT.grId <$> dynGameRecord
  divClass "chat-messages" $ do
    simpleList (filterMessages <$> dynChatMessages <*> dynIsBlack)
      (\dynMessage -> do
          let className =
                (("class" =:) . ("chat-message-" <>) . T.pack . show . OT.chatMessageSenderType) <$> dynMessage
          elDynAttr "div" className $ dynText $
            (\message ->
               T.pack (show (OT.chatMessageSenderType message)) <>
               ": " <>
               OT.chatMessageContent message) <$>
            dynMessage)
  divClass "chat-send-bar" $ do
    evValue <- inputW
    dynValue <- holdDyn "" evValue
    let dynSendInput = flip UserInput.ChatMessage shared <$> dynValue
        evFire = () <$ evValue
    fmapMaybe reqSuccess <$> SC.sendMessage (Right <$> dynGameId) ( Right <$> dynSendInput) evFire
  pure ()

rightFilter messages isBlack =
    filter (\message -> case isBlack of
                True ->
                  OT.chatMessageSenderType message `elem` [WhitePlayer, WhiteTeacher] ||
                  (OT.chatMessageSenderType message `elem` [BlackPlayer, BlackTeacher] &&
                  OT.chatMessageShared message )
                False ->
                  OT.chatMessageSenderType message `elem` [BlackPlayer,BlackTeacher] ||
                  (OT.chatMessageSenderType message `elem` [WhitePlayer, WhiteTeacher] &&
                  OT.chatMessageShared message )
            ) messages

leftFilter messages isBlack =
    filter (\message -> case isBlack of
                True ->
                  OT.chatMessageSenderType message `elem` [BlackPlayer,BlackTeacher] &&
                  ( not $ OT.chatMessageShared message)
                False ->
                  OT.chatMessageSenderType message `elem` [WhitePlayer, WhiteTeacher] &&
                  ( not $ OT.chatMessageShared message)
            ) messages

-- output an input text widget with auto clean on return and return an
-- event firing on return containing the string before clean
inputW ::  forall t m.  MonadWidget t m => m (Event t T.Text)
inputW = do
  rec
    let send = keypress Enter input
        -- send signal firing on *return* key press
    input <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") send
    -- inputElement with content reset on send
  return $ tag (current $ _inputElement_value input) send
