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

module ProfilePage where

import           Data.Text      (Text)
import           Game           (boardPositions)
import qualified Game           as G
import qualified GameDB
import qualified GameLogic      as GL
import           PageUtil
import           Proofs
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient  as SC
import           Theory.Named

profilePage :: forall t m. MonadWidget t m =>
             Dynamic t Page
          -> m (Dynamic t [Event t ()])
profilePage dynPage = elDynAttr "div" (shouldShow Profile "profile-page" <$> dynPage) $ do
  let evPage = updated dynPage
      bvIsProfile = (== Profile) <$> current dynPage
      evProfilePage = () <$ gate bvIsProfile evPage
  evAllGames <- fmapMaybe reqSuccess <$> SC.gamesForProfile evProfilePage
  let evGameRecords = fst <$> evAllGames
  dynGames <- foldDyn (\gr _ -> fmap GameDB._game gr) [] evGameRecords
  profileBoards dynGames

profileBoards :: forall t m. MonadWidget t m =>
                 Dynamic t [G.Game]
              -> m (Dynamic t [Event t ()])
profileBoards dynGames = divClass "profile-boards" $ simpleList dynGames readOnlyBoard

readOnlyBoard :: forall t m . MonadWidget t m =>
                 Dynamic t G.Game
              -> m (Event t ())
readOnlyBoard dynGame = do
  _ <- divClass "readonly-board" $ mapM (\pos -> name pos $
                                \case
                                    Bound boundPos -> boardButton pos $
                                      GL.getPosition boundPos <$> dynGame
                                    _ -> error "unbound position when creating readonly-boardEl")
                              (concat boardPositions)
  readOnlyBoardButton dynGame
