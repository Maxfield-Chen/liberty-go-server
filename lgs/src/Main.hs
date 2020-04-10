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

import           Control.Monad.Except                (liftIO)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Text                           (Text)
import           Game
import           GameApiImpl
import           GameDB                              hiding (User)
import           GameExpressions
import           GHC.Generics                        (Generic)
import           Network.HTTP.Media                  ((//), (/:))
import           Network.Wai.Handler.Warp            (run)
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import qualified UserInput

type Unprotected = "users" :> "register"
                :> ReqBody '[JSON] UserInput.User
                :> Post '[JSON] ()

              :<|> "users" :> "login"
                :> ReqBody '[JSON] UserInput.Login
                :> Verb 'POST 204 '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                    , Header "Set-Cookie" SetCookie]
                                                    NoContent)

              :<|> "users" :> Capture "userId" Int :> "games"
                :> Get '[JSON] [GameRecord]


type GameAPI = "play" :> "proposeGame"
                :> ReqBody '[JSON] UserInput.ProposedGame
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

protected :: Servant.Auth.Server.AuthResult UserInput.User -> Server GameAPI
protected (Servant.Auth.Server.Authenticated user) =
  proposeGame user  :<|>
  gameOperations user


gameOperations user gameId =
  getGameId gameId :<|>
  acceptGameProposal gameId :<|>
  updatePassProposal gameId :<|>
  proposeTerritory gameId :<|>
  acceptTerritoryProposal gameId :<|>
  placeStone gameId


type API auths = (Servant.Auth.Server.Auth auths UserInput.User :> GameAPI) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cookieSettings jwtSettings = protected :<|> unprotected cookieSettings jwtSettings

main :: IO ()
main = do
  signingKey <- generateKey
  let jwtCfg = defaultJWTSettings signingKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[Cookie])
  run 8888 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)

--TODO: Hash password before lookup
checkCreds :: CookieSettings
           -> JWTSettings
           -> UserInput.Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                                NoContent)
checkCreds cookieSettings jwtSettings (UserInput.Login name pass) = do
  mUser <- liftIO $ getUserViaCreds name pass
  case mUser of
    Nothing -> throwError err401
    Just user -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> pure $ applyCookies NoContent
