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

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map            as Map
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

type UserId = Int

--TODO: Investigate evProfilePage - currently submitting on every page change.
-- Gate was not submitting until profile was clicked twice. Investigate other behavior
profilePage :: forall t m. MonadWidget t m =>
             Dynamic t Page
          -> m (Event t Int)
profilePage dynPage = elDynAttr "div" (shouldShow Profile "profile-page" <$> dynPage) $ do
  let evProfilePage = () <$ updated dynPage
  evAllGames <- fmapMaybe reqSuccess <$> SC.gamesForProfile evProfilePage
  evUser <- fmapMaybe reqSuccess <$> SC.userForProfile evProfilePage
  dynAllGames <- holdDyn ([],mempty) evAllGames
  dynUserId <- holdDyn (-1) $ OT.userId <$> evUser
  profileBoards dynAllGames dynUserId


profileBoards :: forall t m. MonadWidget t m =>
                 Dynamic t OT.AllGames
              -> Dynamic t UserId
              -> m (Event t Int)
profileBoards dynAllGames dynUserId =
  let dynGames = (\(grs, awaiters) ->
                    filter ((\gs -> case gs of
                                 G.TerritoryAccepted -> False
                                 G.GameRejected      -> False
                                 _                   -> True)
                             . G._status . OT.grGame . fst) $
                      (\gr -> (gr, HashMap.lookupDefault [] (OT.grId gr) awaiters)) <$>
                    grs)
                 <$> dynAllGames
  in do
    divClass "profile-lpad" $ text ""
  --TODO: Add Pagination / truncation at 14 boards (CSS restriction)
    dynEvents <- divClass "profile-boards" $ simpleList dynGames (readOnlyBoard dynUserId)
    divClass "profile-rpad" $ text ""
    pure $ switchDyn $ leftmost <$> dynEvents


readOnlyBoard :: forall t m . MonadWidget t m =>
                 Dynamic t UserId
              -> Dynamic t (OT.GameRecord, [OT.Awaiter])
              -> m (Event t Int)
readOnlyBoard dynUserId dynAllGame = do
  let dynGameRecord = fst <$> dynAllGame
      dynAwaiters = snd <$> dynAllGame
      dynBlackPlayerAwaiter = OT.userAwaiting OT.grBlackPlayer <$> dynGameRecord <*> dynAwaiters
      dynWhitePlayerAwaiter = OT.userAwaiting OT.grWhitePlayer <$> dynGameRecord <*> dynAwaiters
      dynMBlackTeacherAwaiter = OT.teacherAwaiting OT.grBlackTeacher <$> dynGameRecord <*> dynAwaiters
      dynMWhiteTeacherAwaiter = OT.teacherAwaiting OT.grWhiteTeacher <$> dynGameRecord <*> dynAwaiters
      dynGame = OT.grGame <$> dynGameRecord
  divClass "read-only-container" $ do
    divClass "board-top" $ text ""
    divClass "board-left" $ text ""
    selBoard <- divClass "ro-board-canvas" $ divClass "board-overlay" $ divClass "ro-board-grid" $ do
      _ <- mapM (\pos -> name pos $
         \case
             Bound boundPos -> boardButton pos $
               GL.getPosition boundPos <$> dynGame
             _ -> error "unbound position when creating readonly-boardEl")
       (concat boardPositions)
      readOnlyBoardButton dynGameRecord
    divClass "board-right" $ text ""
    divClass "board-bottom" $ text ""
    _ <- divClass "board-footer" $ do
      apb <- acceptGameProposalButton dynAllGame dynUserId
      rpb <- rejectGameProposalButton dynAllGame dynUserId
      divClass "awaiters" $ do
        bpAwaiter <- awaiterButton (Just <$> dynBlackPlayerAwaiter) "b-awaiter-student" Profile
        btAwaiter <- awaiterButton dynMBlackTeacherAwaiter "b-awaiter-teacher" Profile
        wpAwaiter <- awaiterButton (Just <$> dynWhitePlayerAwaiter) "w-awaiter-student" Profile
        wtAwaiter <- awaiterButton dynMWhiteTeacherAwaiter "w-awaiter-teacher" Profile
        pure $ leftmost [bpAwaiter, btAwaiter, wpAwaiter, wtAwaiter]
    pure selBoard

acceptGameProposalButton :: forall t m. MonadWidget t m =>
                            Dynamic t (OT.GameRecord, [OT.Awaiter])
                         -> Dynamic t UserId
                         -> m (Event t (Maybe G.GameStatus))
acceptGameProposalButton dynAllGame dynUserId = do
  let dynGameId = Right . OT.grId . fst <$> dynAllGame
  (btn, _) <- elDynAttr' "button"
    (ffor2 dynAllGame dynUserId (styleProposal "accept-reject-game-proposal-button"))
    $ dynText "Accept Game Proposal"
  fmapMaybe reqSuccess <$>
    SC.acceptGameProposal dynGameId (constDyn $ Right True) (domEvent Click btn)

rejectGameProposalButton :: forall t m. MonadWidget t m =>
                            Dynamic t (OT.GameRecord, [OT.Awaiter])
                         -> Dynamic t UserId
                         -> m (Event t (Maybe G.GameStatus))
rejectGameProposalButton dynAllGame dynUserId = do
  let dynGameId = Right . OT.grId . fst <$> dynAllGame
  (btn, _) <- elDynAttr' "button"
    (ffor2 dynAllGame dynUserId (styleProposal "reject-game-proposal-button"))
    $ dynText "Reject Game Proposal"
  fmapMaybe reqSuccess <$>
    SC.acceptGameProposal dynGameId (constDyn $ Right False) (domEvent Click btn)

styleProposal :: Text
              -> (OT.GameRecord, [OT.Awaiter])
              -> UserId
              -> Map.Map Text Text
styleProposal _ (_, []) _ = "class" =: "page-hidden"
styleProposal className (_, awaiters) userId =
  case userId `elem` (OT.awaiterUser <$> awaiters) of
    False -> "class" =: "page-hidden"
    True  -> "class" =: className
