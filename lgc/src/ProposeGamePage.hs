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
proposeGamePage dynPage = elDynAttr "div" (shouldShow ProposeGame "register-page" <$> dynPage) $
  divClass "form-prompt" $ do
    dynMBlackPlayer :: Dynamic t (Maybe Int) <- divClass "propose-game" $ do
      text "Black Player"
      el "br" blank
      fmap (readMaybe . unpack) . value <$> inputElement def
    el "br" blank
    dynMWhitePlayer :: Dynamic t (Maybe Int) <- divClass "propose-game-" $ do
      text "White Player"
      el "br" blank
      fmap (readMaybe . unpack) . value <$> inputElement def
    el "br" blank
    blackTeacher :: Dynamic t (Maybe Int) <- divClass "propose-game-" $ do
      text "Black Teacher"
      el "br" blank
      fmap (readMaybe . unpack) . value <$> inputElement def
    el "br" blank
    whiteTeacher :: Dynamic t (Maybe Int) <- divClass "propose-game-" $ do
      text "White Teacher"
      el "br" blank
      fmap (readMaybe . unpack) . value <$> inputElement def
    el "br" blank
    blackFocus :: Dynamic t Text <- divClass "propose-game-" $ do
      text "Black Focus"
      el "br" blank
      value <$> inputElement def
    el "br" blank
    whiteFocus :: Dynamic t Text <- divClass "propose-game-" $ do
      text "White Focus"
      el "br" blank
      value <$> inputElement def
    b <- divClass "submit-button" $ button "Propose Game"
    let userDyn = Right <$> (UserInput.ProposedGame <$>
                            (fromMaybe (-1) <$> dynMBlackPlayer) <*>
                            (fromMaybe (-1) <$> dynMWhitePlayer) <*>
                            blackTeacher <*>
                            whiteTeacher <*>
                            blackFocus <*>
                            whiteFocus)
    proposeEv <- fmapMaybe reqSuccess <$> SC.proposeGame userDyn b
    pure b
