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
import GameDB
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

data NewGameStatus = Accepted | Rejected | Proposed | UserNotFound

type GameAPI ="users" :> "register"
                :> ReqBody '[JSON] Text :> ReqBody '[JSON] Text :> ReqBody '[JSON] Text
                :> Post '[JSON] ()

                :<|> "users" :> Capture "userId" Int :> "games"
                :> Post '[JSON] [GameRecord]

              :<|> "play" :> Capture "gameId" Int
                :> Get '[JSON] (Maybe GameRecord)

              :<|> "play" :> "newGame"
                :> ReqBody '[JSON] Int :> ReqBody '[JSON] Int
                :> Post '[JSON] ()

              :<|> "play" :> Capture "gameId" Int :> "acceptGameProposal"
                :> ReqBody '[JSON] Bool
                :> Post '[JSON] (Either MoveError GameStatus)

              :<|> "play" :> Capture "gameId" Int :> "placeStone"
                :> ReqBody '[JSON] Position
                :> Put '[JSON] ((Either MoveError Outcome), Game)

              :<|> "play" :> Capture "gameId" Int :> "proposeCounting"
                :> ReqBody '[JSON] Bool
                :> Put '[JSON] (Either MoveError GameStatus)

              :<|> "play" :> Capture "gameId" Int :> "proposeTerritory"
                :> ReqBody '[JSON] Bool
                :> ReqBody '[JSON] Territory
                :> Put '[JSON] (Either MoveError GameStatus)

-- TODO: Extract all configuration variables into a common file
dbFilename = "LGS.db"

server1 :: Server GameAPI
server1 =
  createNewUser :<|>
  getGamesForPlayer :<|>
  getGameId :<|>
  createNewGame  :<|>
  proposeGame :<|>
  placeStone :<|>
  proposeCounting :<|>
  proposeTerritory

gameAPI :: Proxy GameAPI
gameAPI = Proxy

app1 :: Application
app1 = serve gameAPI server1

main :: IO ()
main = run 9999 app1
