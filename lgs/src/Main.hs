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
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html


type GameAPI ="users" :> "register"
                :> ReqBody '[JSON] User
                :> Post '[JSON] ()

              :<|> "users" :> Capture "userId" Int :> "games"
                :> Get '[JSON] [GameRecord]

              :<|> "play" :> "proposeGame"
                :> ReqBody '[JSON] (Int,Int)
                :> Post '[JSON] ()

              :<|> "play" :> (Capture "gameId" Int :>
                (Get '[JSON] (Maybe GameRecord)
                :<|>  "acceptGameProposal" :> ReqBody '[JSON] Bool
                                           :> Post '[JSON] (Maybe GameStatus)

                :<|> "proposeCounting" :> Put '[JSON] (Maybe GameStatus)

                :<|>  "acceptCountingProposal" :> ReqBody '[JSON] Bool
                                               :> Put '[JSON] (Maybe GameStatus)

                :<|> "proposeTerritory" :> ReqBody '[JSON] Territory
                                        :> Put '[JSON] (Maybe GameStatus)

                :<|> "acceptTerritoryProposal" :> ReqBody '[JSON] Bool
                                               :> Put '[JSON] (Maybe GameStatus)

                :<|> "placeStone" :> ReqBody '[JSON] Position
                                  :> Put '[JSON] ((Either MoveError Outcome),Game)))

lgsServer :: Server GameAPI
lgsServer =
  createNewUser :<|>
  getGamesForPlayer :<|>
  proposeGame  :<|>
  gameOperations

gameOperations gameId =
  getGameId gameId :<|>
  acceptGameProposal gameId :<|>
  proposeCounting gameId :<|>
  acceptCountingProposal gameId :<|>
  proposeTerritory gameId :<|>
  acceptTerritoryProposal gameId :<|>
  placeStone gameId

gameAPI :: Proxy GameAPI
gameAPI = Proxy

app1 :: Application
app1 = serve gameAPI lgsServer

main :: IO ()
main = run 9999 app1
