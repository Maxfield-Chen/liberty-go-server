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

module ProposeGamePage where

import           Data.Maybe     (fromMaybe)
import           Data.Text      (Text, unpack)
import           PageUtil
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient  as SC
import           Text.Read      (readMaybe)
import qualified UserInput

proposeGamePage :: forall t m. MonadWidget t m =>
             Dynamic t Page
          -> m (Event t ())
proposeGamePage dynPage = elDynAttr "div" (shouldShow ProposeGame "register-page" <$> dynPage) $ do
  text "Black Player"
  dynMBlackPlayer :: Dynamic t (Maybe Int) <-
    fmap (readMaybe . unpack) . value <$> inputElement def
  text "White Player"
  dynMWhitePlayer :: Dynamic t (Maybe Int) <-
    fmap (readMaybe . unpack) . value <$> inputElement def
  text "Black Teacher"
  blackTeacher :: Dynamic t (Maybe Int) <-
    fmap (readMaybe . unpack) . value <$> inputElement def
  text "White Teacher"
  whiteTeacher :: Dynamic t (Maybe Int) <-
    fmap (readMaybe . unpack) . value <$> inputElement def
  text "Black Focus"
  blackFocus :: Dynamic t Text <-
    value <$> inputElement def
  text "White Focus"
  whiteFocus :: Dynamic t Text <-
    value <$> inputElement def
  b <- button "Propose Game"
  let userDyn = Right <$> (UserInput.ProposedGame <$>
                          (fromMaybe (-1) <$> dynMBlackPlayer) <*>
                          (fromMaybe (-1) <$> dynMWhitePlayer) <*>
                          blackTeacher <*>
                          whiteTeacher <*>
                          blackFocus <*>
                          whiteFocus)
  proposeEv <- fmapMaybe reqSuccess <$> SC.proposeGame userDyn b
  pure b
