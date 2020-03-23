{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.String.Conversions
import GHC.Generics
import Lucid
import Game
import GameDB hiding (User)
import GameApiImpl
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


  -- TODO: Factor out common API endpoints
type GameAPI ="users" :> "register"
                :> ReqBody '[JSON] User
                :> Post '[JSON] ()

              :<|> "users" :> Capture "userId" Int :> "games"
                :> Post '[JSON] [GameRecord]

              :<|> "play" :> "proposeGame"
                :> ReqBody '[JSON] (Int,Int)
                :> Post '[JSON] ()

              :<|> "play" :> Capture "gameId" Int
                :> Get '[JSON] (Maybe GameRecord)

              :<|> "play" :> Capture "gameId" Int :> "acceptGameProposal"
                :> ReqBody '[JSON] Bool
                :> Post '[JSON] (Maybe GameStatus)

              :<|> "play" :> Capture "gameId" Int :> "proposeCounting"
                :> Put '[JSON] (Maybe GameStatus)

              :<|> "play" :> Capture "gameId" Int :> "acceptCountingProposal"
                :> ReqBody '[JSON] Bool
                :> Put '[JSON] (Maybe GameStatus)

              :<|> "play" :> Capture "gameId" Int :> "proposeTerritory"
                :> ReqBody '[JSON] Territory
                :> Put '[JSON] (Maybe GameStatus)

              :<|> "play" :> Capture "gameId" Int :> "acceptTerritoryProposal"
                :> ReqBody '[JSON] Bool
                :> Put '[JSON] (Maybe GameStatus)

              :<|> "play" :> Capture "gameId" Int :> "placeStone"
                :> ReqBody '[JSON] Position
                :> Put '[JSON] ((Either MoveError Outcome),Game)

-- TODO: Extract all configuration variables into a common file
dbFilename = "LGS.db"

server1 :: Server GameAPI
server1 =
  createNewUser :<|>
  getGamesForPlayer :<|>
  proposeGame  :<|>
  getGameId :<|>
  acceptGameProposal :<|>
  proposeCounting :<|>
  acceptCountingProposal :<|>
  proposeTerritory :<|>
  acceptTerritoryProposal :<|>
  placeStone

gameAPI :: Proxy GameAPI
gameAPI = Proxy

app1 :: Application
app1 = serve gameAPI server1

main :: IO ()
main = run 9999 app1
