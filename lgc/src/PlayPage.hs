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
import           OutputTypes
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
         -> m ()
playPage dynPage dynGameId =
  elDynAttr "div" (shouldShow Play "play-page" <$> dynPage) $ do
    dynText $ T.pack . show <$> dynGameId
    let evGetGame = updated dynGameId
    evMGameRecord <- getGameEl dynGameId (() <$ evGetGame)
    dynGame       <- holdDyn newGame $ fromMaybe newGame <$> evMGameRecord
    boardEv       <- boardEl dynGame
    posDyn        <- holdDyn (Left "No Pos") $ Right <$> boardEv
    _ <- fmapMaybe reqSuccess <$>
      SC.placeStone (Right <$> dynGameId) posDyn (() <$ boardEv)
    pure ()


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

getGameEl :: forall t m. MonadWidget t m =>
             Dynamic t GameId
          -> Event t ()
          -> m (Event t (Maybe G.Game))
getGameEl dynGameId evGetGame = do
  evFetchMGR <- fmapMaybe reqSuccess <$> SC.getGame (Right  <$> dynGameId) evGetGame
  let evMFetchGame :: Event t (Maybe G.Game) = fmap grGame <$> evFetchMGR
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
  pure $ getGameFromUpdate <$> evMGameMessage

getGameFromUpdate :: Maybe GameMessage -> Maybe G.Game
getGameFromUpdate = (=<<) (\case
                              UpdateGame g -> Just g
                              _            -> Nothing)
