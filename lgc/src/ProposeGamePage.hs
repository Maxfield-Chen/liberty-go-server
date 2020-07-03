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
    dynMBlackPlayer :: Dynamic t (Maybe Int) <- divClass "propose-game" $
      fmap (readMaybe . unpack) . value <$> placeHolderInput "Black Player"
    dynMWhitePlayer :: Dynamic t (Maybe Int) <- divClass "propose-game-" $
      fmap (readMaybe . unpack) . value <$> placeHolderInput "White Player"
    blackTeacher :: Dynamic t (Maybe Int) <- divClass "propose-game-" $
      fmap (readMaybe . unpack) . value <$> placeHolderInput "Black Teacher"
    whiteTeacher :: Dynamic t (Maybe Int) <- divClass "propose-game-" $
      fmap (readMaybe . unpack) . value <$> placeHolderInput "White Teacher"
    blackFocus :: Dynamic t Text <- divClass "propose-game-" $
      value <$> placeHolderInput "Black Focus"
    whiteFocus :: Dynamic t Text <- divClass "propose-game-" $
      value <$> placeHolderInput "White Focus"
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
