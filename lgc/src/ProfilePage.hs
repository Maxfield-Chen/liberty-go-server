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

import qualified Data.HashMap.Strict as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Game                (boardPositions)
import qualified Game                as G
import qualified GameLogic           as GL
import qualified OutputTypes         as OT
import           PageUtil
import           Proofs
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient       as SC
import           Theory.Named

profilePage :: forall t m. MonadWidget t m =>
             Dynamic t Page
          -> m (Dynamic t [Event t ()])
profilePage dynPage = elDynAttr "div" (shouldShow Profile "profile-page" <$> dynPage) $ do
  let evPage = updated dynPage
      bvIsProfile = (== Profile) <$> current dynPage
      evProfilePage = () <$ gate bvIsProfile evPage
  evAllGames <- fmapMaybe reqSuccess <$> SC.gamesForProfile evProfilePage
  dynAllGames <- holdDyn ([],mempty) evAllGames
  profileBoards dynAllGames

profileBoards :: forall t m. MonadWidget t m =>
                 Dynamic t OT.AllGames
              -> m (Dynamic t [Event t ()])
profileBoards dynAllGames =
  let dynGames = (\(grs, awaiters) ->
                    (\gr -> (OT.grGame gr, M.lookupDefault [] (OT.grId gr) awaiters)) <$>
                    grs)
                 <$> dynAllGames
  in divClass "profile-boards" $ simpleList dynGames readOnlyBoard

readOnlyBoard :: forall t m . MonadWidget t m =>
                 Dynamic t (G.Game, [OT.Awaiter])
              -> m (Event t ())
readOnlyBoard dynAllGame = do
  let dynGame = fst <$> dynAllGame
      dynNumAwaiters = T.pack . show . length . snd <$> dynAllGame
  dynText dynNumAwaiters
  _ <- divClass "readonly-board" $ mapM (\pos -> name pos $
                                \case
                                    Bound boundPos -> boardButton pos $
                                      GL.getPosition boundPos <$> dynGame
                                    _ -> error "unbound position when creating readonly-boardEl")
                              (concat boardPositions)
  readOnlyBoardButton dynGame
