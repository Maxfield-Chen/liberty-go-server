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
import           PageUtil
import           Proofs
import           PubSubTypes          hiding (GameId, gameId)
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient        as SC
import           Text.Read            (readMaybe)
import           Theory.Named


type GameId = Int

playPage :: forall t m . MonadWidget t m =>
            Dynamic t Page
         -> Dynamic t GameId
         -> m (Event t Page)
playPage dynPage dynGameId =
  elDynAttr "div" (shouldShow Play "play-page" <$> dynPage) $ do
    dynText $ T.pack . show <$> dynGameId
    let evGetGame = updated dynGameId
        evEmptyGetGame = () <$ evGetGame
    evFetchMGR <- fmapMaybe reqSuccess <$> SC.getGame (Right  <$> dynGameId) evEmptyGetGame
    evUser <- fmapMaybe reqSuccess <$> SC.userForProfile evEmptyGetGame
    dynUser <- holdDyn OT.newUser evUser
    evMGame <- getGame dynGameId (fmap OT.grGame <$> evFetchMGR) evEmptyGetGame
    dynGame <- holdDyn newGame $ fromMaybe newGame <$> evMGame
    dynGR <- holdDyn OT.newGameRecord (fromMaybe OT.newGameRecord <$> evFetchMGR)
    evPlayerPage <- playerSidebar dynGR dynUser
    boardEv       <- boardEl dynGame
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
    divClass "board-grid" $ do
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

getGame :: forall t m. MonadWidget t m =>
             Dynamic t GameId
          -> Event t (Maybe G.Game)
          -> Event t ()
          -> m (Event t (Maybe G.Game))
getGame dynGameId evMFetchGame evGetGame = do
  evMWSGame <- realTimeEl dynGameId evGetGame
  pure $ mergeWith (\mws mhttp -> case mws of
                       Just ws -> Just ws
                       Nothing -> mhttp) [evMWSGame, evMFetchGame]


realTimeEl :: forall t m. MonadWidget t m =>
              Dynamic t GameId
           -> Event t ()
           -> m (Event t (Maybe G.Game))
realTimeEl dynGameId b = do
  text "Arb WS Message"
  rec wsReq <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") arbMessage
      let arbMessage = fmap encodeUtf8 $ tag (current $ value wsReq) $ keypress Enter wsReq
          joinMessage = (encodeUtf8 . (<> "}") . ("{\"type\": \"join\",\"gameId\": " <>)
                         . T.pack . show )
                         <$> tagPromptlyDyn dynGameId b
          newMessage = (:[]) <$> leftmost [joinMessage, arbMessage]
      evMGameMessage :: Event t (Maybe GameMessage) <- do
        ws <- webSocket "ws://localhost:8888" $ def &
          webSocketConfig_send .~ newMessage
        pure $ decode <$> BL.fromStrict <$> _webSocket_recv ws
      dynMGameMessage <- holdDyn (Just $ UpdateGame OT.newGameUpdate) evMGameMessage
  pure $ updated (getGameFromUpdate <$> dynGameId <*> dynMGameMessage)

getGameFromUpdate :: Int -> Maybe GameMessage -> Maybe G.Game
getGameFromUpdate gameId = (=<<) (\case
                              UpdateGame (OT.GameUpdate gid g) -> case gid == gameId of
                                True  -> Just g
                                False -> Nothing
                              _            -> Nothing)
