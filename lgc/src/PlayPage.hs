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
import           GameDB
import qualified GameLogic            as GL
import           PageUtil
import           Proofs
import           PubSubTypes          hiding (gameId)
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient        as SC
import           Text.Read            (readMaybe)
import           Theory.Named


playPage :: forall t m . MonadWidget t m =>
            Dynamic t Page
         -> m ()
playPage dynPage =
  elDynAttr "div" (shouldShow Play "play-page" <$> dynPage) $ do
    gameId :: Dynamic t (Maybe Int) <-
      fmap (readMaybe . T.unpack) . value <$> textInput def
    evMGameRecord <- getGameEl gameId
    dynGame       <- holdDyn newGame $ fromMaybe newGame <$> evMGameRecord
    boardEv       <- boardEl dynPage dynGame
    posDyn        <- holdDyn (Left "No Pos") $ Right <$> boardEv
    _ <- fmapMaybe reqSuccess <$>
      SC.placeStone (Right . fromMaybe (-1) <$> gameId) posDyn (() <$ boardEv)
    pure ()

boardEl :: forall t m . MonadWidget t m =>
           Dynamic t Page
        -> Dynamic t G.Game
        -> m (Event t Position)
boardEl dynPage dynGame =
    elDynAttr "div" (shouldShow Play "board-grid" <$> dynPage) $ do
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

getGameEl :: forall t m. MonadWidget t m =>
             Dynamic t (Maybe Int)
          -> m (Event t (Maybe G.Game))
getGameEl gameId = do
  b <- button "Retrieve Game"
  evFetchMGR <- fmapMaybe reqSuccess <$> SC.getGame (Right . fromMaybe (-1) <$> gameId ) b
  let evMFetchGame :: Event t (Maybe G.Game) = fmap _game <$> evFetchMGR
  evMWSGame <- realTimeEl gameId b
  pure $ mergeWith (\mws mhttp -> case mws of
                       Just ws -> Just ws
                       Nothing -> mhttp) [evMWSGame, evMFetchGame]


realTimeEl :: forall t m. MonadWidget t m =>
              Dynamic t (Maybe Int)
           -> Event t ()
           -> m (Event t (Maybe G.Game))
realTimeEl gameId b = do
  rec wsReq <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") arbMessage
      let arbMessage = fmap encodeUtf8 $ tag (current $ value wsReq) $ keypress Enter wsReq
          joinMessage = (encodeUtf8 . (<> "}") . ("{\"type\": \"join\",\"gameId\": " <>) . T.pack . show . fromMaybe (-1))
            <$> tagPromptlyDyn gameId b
          newMessage = (:[]) <$> leftmost [joinMessage, arbMessage]
      evMGameMessage :: Event t (Maybe GameMessage) <- do
        ws <- webSocket "ws://localhost:8888" $ def &
          webSocketConfig_send .~ newMessage
        pure $ decode <$> BL.fromStrict <$> _webSocket_recv ws
  pure $ getGameFromUpdate <$> evMGameMessage

getGameFromUpdate :: Maybe GameMessage -> Maybe G.Game
getGameFromUpdate = (=<<) (\case
                              UpdateGame g -> Just g
                              _            -> Nothing)
