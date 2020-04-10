{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module GameApiImpl where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                  (Text)
import           Game
import           GameDB                     hiding (User)
import           GameExpressions
import qualified GameLogic
import qualified GameLogic                  as GL
import           GHC.Generics
import           Prelude                    ()
import           Prelude.Compat
import           Proofs
import           Servant
import           Theory.Named

data User =
  User
    { userEmail    :: Text
    , userName     :: Text
    , userPassword :: Text
    } deriving (Generic, ToJSON, FromJSON)

data ProposedGame =
  ProposedGame {_pg_black_player :: Int,
           _pg_white_player      :: Int,
           _pg_black_teacher     :: Maybe Int,
           _pg_white_teacher     :: Maybe Int,
           _pg_black_focus       :: Text,
           _pg_white_focus       :: Text
           } deriving (Generic, ToJSON, FromJSON)

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

proposeGame :: ProposedGame -> Handler ()
proposeGame g = do liftIO (insertGame (_pg_black_player g) (_pg_white_player g) (_pg_black_teacher g) (_pg_white_teacher g) (_pg_black_focus g) (_pg_white_focus g) newGame)

acceptGameProposal :: Int -> Bool -> Handler (Maybe GameStatus)
acceptGameProposal gameId shouldAccept = liftIO $ updateGameProposal gameId shouldAccept

updatePassProposal :: Int -> Space -> Handler (Maybe GameStatus)
updatePassProposal gameId space = do
  mGame <- liftIO $ updateGame (GL.proposePass space) gameId
  case mGame of
    Just game -> pure (Just (game ^. status))
    Nothing   -> pure Nothing

proposeTerritory :: Int -> Territory -> Handler (Maybe GameStatus)
proposeTerritory gameId territory = do
  mGame <- liftIO $ updateGame (GL.proposeTerritory territory) gameId
  case mGame of
    Just game -> pure (Just (game ^. status))
    Nothing   -> pure Nothing

acceptTerritoryProposal :: Int -> Bool -> Handler (Maybe GameStatus)
acceptTerritoryProposal gameId shouldAccept =
  liftIO $ updateTerritoryProposal gameId shouldAccept
