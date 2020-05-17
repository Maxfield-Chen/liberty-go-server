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

import           Control.Lens
import           Data.FileEmbed
import qualified Data.Map       as Map
import           Data.Maybe
import qualified Data.Text      as T
import           Debug.Trace
import           Game
import           GameDB
import qualified GameLogic      as GL
import           Proofs
import           Reflex
import           Reflex.Dom
import           Servant.Reflex
import qualified ServantClient  as SC
import           Text.Read      (readMaybe)
import           Theory.Named
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

bodyEl :: forall t m . MonadWidget t m => m ()
bodyEl = do
      evMGameRecord <- getGameEl
      loginB <- loginEl
      dynGame <- holdDyn newGame $ (maybe newGame _game) <$> evMGameRecord
      boardEv <- boardEl dynGame
      posDyn <- holdDyn (Left "No Pos") $ Right <$> boardEv
      evOutcome <- fmapMaybe reqSuccess <$>
        SC.placeStone (constDyn (Right 20)) posDyn (() <$ boardEv)
      pure ()

boardEl :: forall t m . MonadWidget t m =>
           Dynamic t Game
        -> m (Event t Position)
boardEl dynGame =
    elClass "div" "boardGrid" $ do
      buttonEvs <- foldr (\pos mButtonEvs -> name pos $
                                    \case
                                        Bound boundPos -> do
                                          let dynSpace = (GL.getPosition boundPos) <$> dynGame
                                          buttonEv <- styledButton pos dynSpace
                                          (:) buttonEv <$> mButtonEvs
                                        _ -> error "unbound position when creating boardEl")
                                  (pure [] :: m [Event t Position])
                                  (concat boardPositions)
      pure $ leftmost buttonEvs

styledButton :: forall t m. MonadWidget t m =>
                Position
             -> Dynamic t Space
             -> m (Event t Position)
styledButton pos dynSpace = do
                (btn, _) <- elDynAttr' "button" (ffor dynSpace styleSpace) $ text ""
                pure $ pos <$ domEvent Click btn

styleSpace :: Space
           -> Map.Map T.Text T.Text
styleSpace space = "class" =: (case space of
                                Game.Empty -> "space-empty"
                                Black      -> "space-black"
                                White      -> "space-white")

loginEl :: forall t m. MonadWidget t m => m (Event t ())
loginEl = do
  userName :: Dynamic t T.Text <-
    value <$> inputElement def
  userPassword :: Dynamic t T.Text <-
    value <$> inputElement def
  b <- button "Login"
  let loginDyn =
        UserInput.Login <$> userName <*> userPassword
  loginEv <- fmapMaybe reqSuccess <$> SC.login (Right <$> loginDyn) b
  pure b


getGameEl :: forall t m. MonadWidget t m => m (Event t (Maybe GameRecord))
getGameEl = do
  rec el "br" blank
      gameId :: Dynamic t (Maybe Int) <-
        fmap (readMaybe . T.unpack) . value <$> textInput def
      b <- button "Retrieve Game"
      evFetchGR <- fmapMaybe reqSuccess <$> SC.getGame (Right . fromMaybe (-1) <$> gameId ) b
      gameRecord <- foldDyn (mappend . T.pack . show) "" evFetchGR
  pure evFetchGR
