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

data NewGameStatus = Accepted | Rejected | Proposed | UserNotFound

type GameAPI = "play" :> Capture "gameId" Int :> ReqBody '[JSON] Game :> Post '[JSON] (Maybe GameRecord)
               :<|> "play" :> "newGame"
                           :> ReqBody '[JSON] Int :> ReqBody '[JSON] Int :> Post '[JSON] ()

dbFilename = "LGS.db"

createNewGame :: Int -> Int -> Handler ()
createNewGame bPlayerId wPlayerId = do
  player <- liftIO (insertGame bPlayerId wPlayerId newGame)
  pure player

server1 :: Server GameAPI
server1 = (pure . liftIO . gameIdToGame) :<|> createNewGame

gameAPI :: Proxy GameAPI
gameAPI = Proxy

app1 :: Application
app1 = serve gameAPI server1

main :: IO ()
main = run 9999 app1
