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
import Control.Lens
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
import qualified GameLogic
import GameDB hiding (User)
import GameExpressions
import qualified GameLogic as GL
import Proofs
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Data.Aeson.Types
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

data User =
  User
    { userEmail :: Text
    , userName :: Text
    , userPassword :: Text
    } deriving Generic

instance ToJSON User
instance FromJSON User

-- TODO: find a more graceful return type when unbound or game not found
placeStone :: Int -> Position -> Handler ((Either MoveError Outcome),Game)
placeStone gameId pos =
  name pos $ \case
    Bound pos -> do
      mGameRecord <- liftIO $ getGameRecord gameId
      case mGameRecord of
        Just gameRecord ->
          let ret@(_, game) =
                runState
                  (runExceptT (GameLogic.placeStone pos))
                  (_game gameRecord)
           in liftIO $ updateGame (const game) gameId >> pure ret
        Nothing -> pure (Left NoBoard, newGame)
    Unbound -> pure (Left OutOfBounds, newGame)

createNewUser :: User -> Handler ()
createNewUser user = liftIO $ insertUser (userEmail user) (userName user) (userPassword user)

getGameId :: Int -> Handler (Maybe GameRecord)
getGameId = liftIO . getGameRecord

getGamesForPlayer :: Int -> Handler [GameRecord]
getGamesForPlayer = liftIO . getGameRecords

proposeGame :: (Int,Int) -> Handler ()
proposeGame (bPlayerId, wPlayerId) = do
  player <- liftIO (insertGame bPlayerId wPlayerId newGame)
  pure player

acceptGameProposal :: Int -> Bool -> Handler (Maybe GameStatus)
acceptGameProposal gameId shouldAccept = liftIO $ updateGameProposal gameId shouldAccept

proposeCounting :: Int -> Handler (Maybe GameStatus)
proposeCounting gameId = do
  mGame <- liftIO $ updateGame GL.proposeCounting gameId
  case mGame of
    Just game -> pure (Just (game ^. status))
    Nothing -> pure Nothing

acceptCountingProposal :: Int -> Bool -> Handler (Maybe GameStatus)
acceptCountingProposal gameId shouldCount = liftIO $ updateCountingProposal gameId shouldCount

proposeTerritory :: Int -> Territory -> Handler (Maybe GameStatus)
proposeTerritory gameId territory = do
  mGame <- liftIO $ updateGame (GL.proposeTerritory territory) gameId
  case mGame of
    Just game -> pure (Just (game ^. status))
    Nothing -> pure Nothing

acceptTerritoryProposal :: Int -> Bool -> Handler (Maybe GameStatus)
acceptTerritoryProposal gameId shouldAccept =
  liftIO $ updateTerritoryProposal gameId shouldAccept
