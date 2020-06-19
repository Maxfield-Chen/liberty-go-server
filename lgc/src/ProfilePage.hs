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
          -> m (Dynamic t [Event t Int])
profilePage dynPage = elDynAttr "div" (shouldShow Profile "profile-page" <$> dynPage) $ do
  bvPage <- hold Profile $ updated dynPage
  let evProfilePage = () <$ gate ((== Profile) <$> bvPage) (updated dynPage)
  evAllGames <- fmapMaybe reqSuccess <$> SC.gamesForProfile evProfilePage
  dynAllGames <- holdDyn ([],mempty) evAllGames
  profileBoards dynAllGames

profileBoards :: forall t m. MonadWidget t m =>
                 Dynamic t OT.AllGames
              -> m (Dynamic t [Event t Int])
profileBoards dynAllGames =
  let dynGames = (\(grs, awaiters) ->
                    (\gr -> (gr, M.lookupDefault [] (OT.grId gr) awaiters)) <$>
                    grs)
                 <$> dynAllGames
  in divClass "profile-boards" $ simpleList dynGames readOnlyBoard

readOnlyBoard :: forall t m . MonadWidget t m =>
                 Dynamic t (OT.GameRecord, [OT.Awaiter])
              -> m (Event t Int)
readOnlyBoard dynAllGame = do
  let dynGameRecord = fst <$> dynAllGame
      dynNumAwaiters = T.pack . show . length . snd <$> dynAllGame
      dynGame = OT.grGame <$> dynGameRecord
  dynText dynNumAwaiters
  _ <- divClass "readonly-board" $ mapM (\pos -> name pos $
                                \case
                                    Bound boundPos -> boardButton pos $
                                      GL.getPosition boundPos <$> dynGame
                                    _ -> error "unbound position when creating readonly-boardEl")
                              (concat boardPositions)
  _ <- acceptGameProposalButton dynAllGame
  _ <- rejectGameProposalButton dynAllGame
  readOnlyBoardButton dynGameRecord


acceptGameProposalButton :: forall t m. MonadWidget t m =>
                            Dynamic t (OT.GameRecord, [OT.Awaiter])
                         -> m (Event t (Maybe G.GameStatus))
acceptGameProposalButton dynAllGame = do
  let dynGameId = Right . OT.grId . fst <$> dynAllGame
  (btn, _) <- elDynAttr' "button" (constDyn $ "class" =: "accept-game-proposal-button")
    $ dynText "Accept Game Proposal"
  fmapMaybe reqSuccess <$>
    SC.acceptGameProposal dynGameId (constDyn $ Right True) (domEvent Click btn)

rejectGameProposalButton :: forall t m. MonadWidget t m =>
                            Dynamic t (OT.GameRecord, [OT.Awaiter])
                         -> m (Event t (Maybe G.GameStatus))
rejectGameProposalButton dynAllGame = do
  let dynGameId = Right . OT.grId . fst <$> dynAllGame
  (btn, _) <- elDynAttr' "button" (constDyn $ "class" =: "reject-game-proposal-button")
    $ dynText "Reject Game Proposal"
  fmapMaybe reqSuccess <$>
    SC.acceptGameProposal dynGameId (constDyn $ Right False) (domEvent Click btn)
