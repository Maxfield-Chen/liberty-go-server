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

import           Control.Lens
import           Data.FileEmbed
import qualified Data.Map       as Map
import           Data.Maybe
import qualified Data.Text      as T
import           Debug.Trace
import           Game
import           GameDB
import qualified GameLogic      as GL
import           Proofs
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient  as SC
import           Text.Read      (readMaybe)
import           Theory.Named
import qualified UserInput

devMain :: IO ()
devMain = mainWidgetWithCss css bodyEl
         where css = $(embedFile "./css/lgs.css")

-- TODO: Determine why this returns 403 when finding css files using jsaddle.
main :: IO ()
main = mainWidgetWithHead headEl bodyEl

headEl :: MonadWidget t m => m ()
headEl = do
  el "title" $ text "LGS"
  styleSheet "./css/lgs.css"
    where styleSheet srcLink = elAttr "link" (Map.fromList [("rel", "stylesheet"), ("type","text/css"), ("href", srcLink)]) $ pure ()

bodyEl :: MonadWidget t m => m ()
bodyEl = do
  rec el "h1" $ text "This should be red."
      evMGameRecord <- getGameEl
      xsEvButtons <- boardRowEl 0 evMGameRecord
  pure ()

-- boardEl :: forall t m. MonadWidget t m => m ()
-- boardEl = divclass "gameBoard" $ do
--   rec

evMPosToSpace :: forall t m. MonadWidget t m =>
                 Position
              -> Event t (Maybe GameRecord)
              -> Event t (Maybe Space)
evMPosToSpace pos evMGameRecord = do
  name pos $ \case
    Bound pos ->
      fmapMaybe (fmap (Just . (GL.getPosition pos) . _game)) evMGameRecord
    Unbound -> Nothing <$ evMGameRecord

boardRowEl :: forall t m. MonadWidget t m => Int -> Event t (Maybe GameRecord) -> m [Event t ()]
boardRowEl offset mEvGameRecord = do
  divClass "boardRow" $ do
    --TODO: Replace offset      VVVVVV with row size from mevgamerecord
    cols <- mapM id $ replicate offset (button "")
    pure cols



getGameEl :: forall t m. MonadWidget t m => m (Event t (Maybe GameRecord))
getGameEl = do
  rec el "br" blank
      gameId :: Dynamic t (Maybe Int) <-
        fmap (readMaybe . T.unpack) . value <$> textInput def
      b <- button "Retrieve Game"
      evFetchGR <- fmapMaybe reqSuccess <$> SC.getGame (Right . fromMaybe (-1) <$> gameId ) b
      gameRecord <- foldDyn (mappend . T.pack . show) "" evFetchGR
      el "div" $ display gameRecord
  pure evFetchGR
