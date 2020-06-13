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

import qualified Data.Map   as M
import           Data.Text  (Text)
import           Game       (Position, Space (..))
import qualified Game       as G
import           Reflex
import           Reflex.Dom


data Page = Main | Register | Login | Play | ProposeGame deriving (Show, Eq)

shouldShow :: Page -> Text -> Page -> M.Map Text Text
shouldShow componentPage componentClass curPage
  | componentPage == curPage = "class" =: componentClass
  | otherwise = "class" =: "page-hidden"

pageButton :: forall t m. MonadWidget t m =>
              Page
           -> Text
           -> m (Event t Page)
pageButton page btnText = do
  (btn,_) <- elDynAttr' "button" mempty $ text btnText
  pure $ page <$ domEvent Click btn

boardButton :: forall t m. MonadWidget t m =>
                Position
             -> Dynamic t Space
             -> m (Event t Position)
boardButton pos dynSpace = do
                (btn, _) <- elDynAttr' "button" (ffor dynSpace styleSpace) $ text ""
                pure $ pos <$ domEvent Click btn

styleSpace :: Space
           -> M.Map Text Text
styleSpace space = "class" =: (case space of
                                G.Empty -> "space-empty"
                                Black   -> "space-black"
                                White   -> "space-white")
