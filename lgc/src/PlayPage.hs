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
import           Data.Bool
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as M
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

    evPlayerPage <- playerSidebar dynGR dynChatMessages dynGame dynUser
    boardEv       <- boardEl $ fromMaybe newGame <$> dynGame
    evOpponentPage <- opponentSidebar dynGR dynChatMessages dynGame dynUser
    posDyn        <- holdDyn (Left "No Pos") $ Right <$> boardEv
    _ <- fmapMaybe reqSuccess <$>
      SC.placeStone (Right <$> dynGameId) posDyn (() <$ boardEv)
    pure $ leftmost [evPlayerPage, evOpponentPage]

opponentSidebar :: forall t m . MonadWidget t m =>
                 Dynamic t OT.GameRecord
              -> Dynamic t [ OT.ChatMessage]
              -> Dynamic t (Maybe G.Game)
              -> Dynamic t OT.User
              -> m (Event t Page)
opponentSidebar dynGameRecord dynChatMessages dynMaybeGame dynProfileUser =
  divClass "sidebar-opponent" $ do
    let dynOpponent = OT.getOpponent <$> dynProfileUser <*> dynGameRecord
        dynMTeacher = OT.getTeacher <$> dynOpponent <*> dynGameRecord
        dynUserColor = bool G.White G.Black <$>
          (OT.isBlack <$> dynOpponent <*> dynGameRecord)
    divClass "sidebar-opponent-color" $
      dynText ((<> " Team") . T.pack . show <$> dynUserColor)
    evPage <- divClass "sidebar-opponent-info" $ do
      evTeacher <- dynClassExtraButton
        (T.pack . show . (\image -> toEnum image :: GDB.ProfileImage) . OT.userImage . fromMaybe OT.newUser <$> dynMTeacher)
        "sidebar-opponent-teacher"
        Profile
      evPlayer <- dynClassExtraButton
        (T.pack . show . (\image -> toEnum image :: GDB.ProfileImage) . OT.userImage <$> dynOpponent)
        "sidebar-opponent-user"
        Profile
      pure $ leftmost [evPlayer, evTeacher]
    evTeacherName <- dynClassTextButton
      (maybe "sidebar-teacher-name-hidden" (const "sidebar-opponent-teacher-name") <$> dynMTeacher)
      (OT.userName . fromMaybe OT.newUser <$> dynMTeacher) Profile
    evPlayerName <- dynTextButton "sidebar-opponent-player-name" (OT.userName <$> dynOpponent) Profile
    divClass "sidebar-separater" blank
    divClass "sidebar-opponent-captures" $
      dynText
        ((\color -> (<> " captures") . T.pack . show . M.findWithDefault 0 color . GL.currentCaptures . fromMaybe newGame)
         <$> dynUserColor <*> dynMaybeGame)
    divClass "sidebar-separater" blank
    divClass "sidebar-opponent-chat" $ chatEl dynGameRecord dynChatMessages dynProfileUser True rightFilter
    pure evPage

playerSidebar :: forall t m . MonadWidget t m =>
                 Dynamic t OT.GameRecord
              -> Dynamic t [ OT.ChatMessage]
              -> Dynamic t (Maybe G.Game)
              -> Dynamic t OT.User
              -> m (Event t Page)
playerSidebar dynGameRecord dynChatMessages dynMaybeGame dynProfileUser =
  divClass "sidebar-player" $ do
    let dynMTeacher = OT.getTeacher <$> dynProfileUser <*> dynGameRecord
        dynProfileUserColor = bool G.White G.Black <$>
          (OT.isBlack <$> dynProfileUser <*> dynGameRecord)
    divClass "sidebar-player-color" $
      dynText ((<> " Team") . T.pack . show <$> dynProfileUserColor)
    evPage <- divClass "sidebar-player-info" $ do
      evPlayer <- dynClassExtraButton
        (T.pack . show . (\image -> toEnum image :: GDB.ProfileImage) . OT.userImage <$> dynProfileUser)
        "sidebar-player-user"
        Profile
      evTeacher <- dynClassExtraButton
        (T.pack . show . (\image -> toEnum image :: GDB.ProfileImage) . OT.userImage . fromMaybe OT.newUser <$> dynMTeacher)
        "sidebar-player-teacher"
        Profile
      pure $ leftmost [evPlayer, evTeacher]
    evPlayerName <- dynTextButton "sidebar-player-name" (OT.userName <$> dynProfileUser) Profile
    evTeacherName <- dynClassTextButton
      (maybe "sidebar-teacher-name-hidden" (const "sidebar-teacher-name") <$> dynMTeacher)
      (OT.userName . fromMaybe OT.newUser <$> dynMTeacher) Profile
    divClass "sidebar-separater" blank
    divClass "sidebar-player-turn" $
      dynText ((<> " to Play.") . T.pack . show . GL.nextToPlay . fromMaybe G.newGame
               <$> dynMaybeGame)
    divClass "sidebar-player-captures" $
      dynText
        ((\color -> (<> " captures") . T.pack . show . M.findWithDefault 0 color . GL.currentCaptures . fromMaybe G.newGame)
         <$> dynProfileUserColor <*> dynMaybeGame)

    evPass <- divClass "sidebar-player-pass" $ button "Pass"
    evPass <- divClass "sidebar-player-request-guidance" $
      dynTextButton
        "sidebar-player-request-guidance-button"
        ((\color ->
            let formatText = (<> " Requests for Guidance") . T.pack . show
            in case color of
              G.Black -> formatText . OT.grBlackGuidanceRemaining
              G.White -> formatText . OT.grWhiteGuidanceRemaining
            ) <$> dynProfileUserColor <*> dynGameRecord)
        ()
    evPassResponse <- fmapMaybe reqSuccess <$> SC.pass (Right . OT.grId <$> dynGameRecord) evPass
    divClass "sidebar-separater" blank
    divClass "sidebar-player-chat" $
      chatEl dynGameRecord dynChatMessages dynProfileUser False leftFilter
    pure evPage

boardEl :: forall t m . MonadWidget t m =>
           Dynamic t G.Game
           -> m (Event t Position)
boardEl dynGame =
  divClass "board-container" $ do
    let letters = "ABCDEFGHJKLMNOPQRST"
    divClass "board-top" $ text letters
    divClass "board-left" $ text "19" >> mapM ((divClass "number-separater" blank >> ) . text . T.pack . show) (reverse [1..18])
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
    divClass "board-right" $ text "19" >> mapM ((divClass "number-separater" blank >> ) . text . T.pack . show) (reverse [1..18])
    divClass "board-bottom" $ text letters
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

getChatMessages :: forall t m. MonadWidget t m =>
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
