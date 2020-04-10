{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Prelude                             ()
import           Prelude.Compat

import           Control.Concurrent                  (forkIO)
import           Control.Monad                       (forever)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans                 (liftIO)
import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                     (ByteString)
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Text                           (Text)
import           Game
import           GameApiImpl
import           GameDB                              hiding (User)
import           GameExpressions
import           GHC.Generics                        (Generic)
import           Network.HTTP.Media                  ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Proofs
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           System.Directory

data Login =
  Login { userName     :: Text,
          userPassword :: Text} deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

type Unprotected = "users" :> "register"
                :> ReqBody '[JSON] User
                :> Post '[JSON] ()

              :<|> "users" :> "login"
                :> ReqBody '[JSON] Login
                :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                    , Header "Set-Cookie" SetCookie]
                                                    NoContent)

              :<|> "users" :> Capture "userId" Int :> "games"
                :> Get '[JSON] [GameRecord]


type GameAPI = "play" :> "proposeGame"
                :> ReqBody '[JSON] ProposedGame
                :> Post '[JSON] ()

              :<|> "play" :> (Capture "gameId" Int :>
                (Get '[JSON] (Maybe GameRecord)
                :<|>  "acceptGameProposal" :> ReqBody '[JSON] Bool
                                           :> Post '[JSON] (Maybe GameStatus)

                :<|> "pass"
                  :> ReqBody '[JSON] Space
                  :> Put '[JSON] (Maybe GameStatus)

                :<|> "proposeTerritory" :> ReqBody '[JSON] Territory
                                        :> Put '[JSON] (Maybe GameStatus)

                :<|> "acceptTerritoryProposal" :> ReqBody '[JSON] Bool
                                               :> Put '[JSON] (Maybe GameStatus)

                :<|> "placeStone" :> ReqBody '[JSON] Position
                                  :> Put '[JSON] ((Either MoveError Outcome),Game)))


unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts = createNewUser
                 :<|> checkCreds cs jwts
                 :<|> getGamesForPlayer

protected :: Servant.Auth.Server.AuthResult User -> Server GameAPI
protected (Servant.Auth.Server.Authenticated user) =
  proposeGame  :<|>
  gameOperations


gameOperations gameId =
  getGameId gameId :<|>
  acceptGameProposal gameId :<|>
  updatePassProposal gameId :<|>
  proposeTerritory gameId :<|>
  acceptTerritoryProposal gameId :<|>
  placeStone gameId


type API auths = (Servant.Auth.Server.Auth auths User :> GameAPI) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cookieSettings jwtSettings = protected :<|> unprotected cookieSettings jwtSettings

main :: IO ()
main = do
  signingKey <- generateKey
  let jwtCfg = defaultJWTSettings signingKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[Cookie])
  run 8888 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)


--TODO: Make this an actual db lookup instead of a mock
checkCreds :: CookieSettings
           -> JWTSettings
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                                NoContent)
checkCreds cookieSettings jwtSettings (Login "name" "pass") = do
  let usr = User "user@email.com" "name" "pass"
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
  case mApplyCookies of
    Nothing           -> throwError err401
    Just applyCookies -> pure $ applyCookies NoContent
checkCreds _ _ _ = throwError err401
