{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GameApiImpl where

import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Data.Maybe
import Data.String.Conversions
import Control.Monad.Trans.Except
import Control.Monad.State
import GHC.Generics
import Game
import Theory.Named
import GameLogic
import GameDB
import GameExpressions
import Proofs
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

createNewUser :: Text -> Text -> Text -> Handler ()
createNewUser email name password = liftIO $ insertUser email name password

createNewGame :: Int -> Int -> Handler ()
createNewGame bPlayerId wPlayerId = do
  player <- liftIO (insertGame bPlayerId wPlayerId newGame)
  pure player

getGameId :: Int -> Handler (Maybe GameRecord)
getGameId = liftIO . gameIdToGame

-- TODO: find a more graceful return type when unbound or game not found
placeStone :: Int -> Position -> Handler ((Either MoveError Outcome),Game)
placeStone gameId pos =
  name pos $ \case
    Bound pos -> do
      mGameRecord <- liftIO (gameIdToGame gameId)
      case mGameRecord of
        Just gameRecord ->
          pure $
          runState (runExceptT (GameLogic.placeStone pos)) (_game gameRecord)
        Nothing -> pure (Left NoBoard, newGame)
    Unbound -> pure (Left OutOfBounds, newGame)
