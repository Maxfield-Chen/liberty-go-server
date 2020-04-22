{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Data.FileEmbed
import qualified Data.Map       as Map
import           Data.Maybe
import qualified Data.Text      as T
import           GameDB
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient  as SC
import           Text.Read      (readMaybe)
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
  _ <- getGameEl
  blank

-- gameBoardEl :: forall t m. MonadWidget t m => m ()
-- gameBoardEl = divclass "gameBoard" $ do
--   rec

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
