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
import Data.List
import Data.Maybe
import Data.String.Conversions
import GHC.Generics
import Lucid
import Game
import GameLogic
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

type GameAPI = "users" :> Get '[JSON] [Game]
                :<|> "id" :> ReqBody '[JSON] Game :> Post '[JSON] Game

server1 :: Server GameAPI
server1 = exampleGame :<|> gameIdentity
  where gameIdentity :: Game -> Handler Game
        gameIdentity game = return game
        exampleGame :: Handler [Game]
        exampleGame = return [newGame]


gameAPI :: Proxy GameAPI
gameAPI = Proxy

app1 :: Application
app1 = serve gameAPI server1

main :: IO ()
main = run 9999 app1
