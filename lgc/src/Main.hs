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
      el "h1" $ text "This should be red."
      evMGameRecord <- getGameEl
      let evMSpace = evMPosToSpace (Pair 0 0) evMGameRecord
      dynButtonAttrs <- styleSpace (fmap (fromMaybe Game.Empty) evMSpace)
      boardButton <- styledButton dynButtonAttrs
      pure ()

evMPosToSpace :: forall t m. MonadWidget t m =>
                 Position
              -> Event t (Maybe GameRecord)
              -> Event t (Maybe Space)
evMPosToSpace pos evMGameRecord = do
  name pos $ \case
    Bound pos ->
      fmapMaybe (fmap (Just . (GL.getPosition pos) . _game)) evMGameRecord
    Unbound -> Nothing <$ evMGameRecord

-- boardRowEl :: MonadWidget t m =>
--                 Int
--              -> [Event t Space]
--              -> [m (Event t ())]
-- boardRowEl offset evSpaces = do
--       dynAttrs <- styleSpace <$> evSpaces
--       styledButton <$> dynAttrs

styledButton :: MonadWidget t m =>
                Dynamic t (Map.Map T.Text T.Text)
             -> m (Event t ())
styledButton dynAttr = do
                (btn, _) <- elDynAttr' "button" dynAttr $ text "X"
                pure $ domEvent Click btn

styleSpace :: forall t m. MonadWidget t m =>
              Event t Space
           -> m (Dynamic t (Map.Map T.Text T.Text))
styleSpace evSpace =
  let spaceToStyle :: Space -> Map.Map T.Text T.Text -> Map.Map T.Text T.Text
      spaceToStyle space _ = "style" =: ("class: " <> case space of
                                  Game.Empty -> "space-empty"
                                  Black      -> "space-black"
                                  White      -> "space-white") in
                         foldDyn spaceToStyle ("style" =: "class: space-empty") evSpace



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
