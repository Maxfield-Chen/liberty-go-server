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

module PageUtil where

import qualified Data.Map    as M
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Game        (Position, Space (..))
import qualified Game        as G
import qualified GameDB
import qualified OutputTypes as OT
import           Reflex
import           Reflex.Dom
import Control.Lens


data Page = Main | Register | Login | Play | Profile | ProposeGame deriving (Show, Eq)

shouldShow :: Page -> Text -> Page -> M.Map Text Text
shouldShow componentPage componentClass curPage
  | componentPage == curPage = "class" =: componentClass
  | otherwise = "class" =: "page-hidden"

pageButton :: forall t m. MonadWidget t m =>
              Page
           -> Text
           -> m (Event t Page)
pageButton page btnText = genButton "page-button" btnText page

-- TODO: Expand this function to account for awaiters per game
readOnlyBoardButton :: forall t m. MonadWidget t m =>
                         Dynamic t OT.GameRecord
                      -> m (Event t Int)
readOnlyBoardButton dynGR = do
  (btn,_) <- elDynAttr' "button" (constDyn $ "class" =: "read-only-board-button")
    $ dynText ""
  pure $ tagPromptlyDyn (OT.grId <$> dynGR) (domEvent Click btn)

genButton :: forall t m a. MonadWidget t m =>
             Text
          -> Text
          -> a
          -> m (Event t a)
genButton className btnText ret = do
  (btn, _) <- elDynAttr' "button" (constDyn $ "class" =: className) $ dynText (constDyn btnText)
  pure $ ret <$ domEvent Click btn

genDynButton :: forall t m a. MonadWidget t m =>
             Text
          -> Dynamic t Text
          -> a
          -> m (Event t a)
genDynButton className btnText ret = do
  (btn, _) <- elDynAttr' "button" (constDyn $ "class" =: className) $ dynText btnText
  pure $ ret <$ domEvent Click btn

awaiterButton :: forall t m a. MonadWidget t m =>
                     Dynamic t (Maybe Bool)
                  -> Text
                  -> a
                  -> m (Event t a)
awaiterButton dynBool className ret = do
  (btn,_) <- elDynAttr' "button" (styleAwaiter <$> dynBool <*> constDyn className) blank
  pure $ ret <$ domEvent Click btn


boardButton :: forall t m. MonadWidget t m =>
                Position
             -> Dynamic t Space
             -> m (Event t Position)
boardButton pos dynSpace = do
                (btn, _) <- elDynAttr' "button" (ffor dynSpace styleSpace) blank
                pure $ pos <$ domEvent Click btn

placeHolderInput :: forall t m . MonadWidget t m =>
                    Text
                 -> m (TextInput  t)
placeHolderInput placeholder = textInput $
  over textInputConfig_attributes (fmap (M.insert "placeholder" placeholder)) def

styleAwaiter :: Maybe Bool -> Text -> M.Map Text Text
styleAwaiter (Just False) className = "class" =: ("approved-" <> className)
styleAwaiter (Just True) className  = "class" =: ("waiting-" <> className)
styleAwaiter Nothing className      = "class" =: ("hide-" <> className)

styleSpace :: Space
           -> M.Map Text Text
styleSpace space = "class" =: (case space of
                                G.Empty -> "space-empty"
                                Black   -> "space-black"
                                White   -> "space-white")
