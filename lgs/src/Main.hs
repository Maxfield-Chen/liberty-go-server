{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Prelude                             ()
import           Prelude.Compat

import           Control.Monad.Except                (liftIO)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Text                           (Text)
import           Game
import           GameApiImpl
import           GameDB
import           GameExpressions
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

              :<|> "play" :> Capture "gameId" Int :> Get '[JSON] (Maybe GameRecord)


type GameAPI = "play" :> "proposeGame"
                :> ReqBody '[JSON] UserInput.ProposedGame
                :> Post '[JSON] GameRecord

              :<|> "play" :> Capture "gameId" Int :>
                ("acceptGameProposal" :> ReqBody '[JSON] Bool
                                     :> Post '[JSON] (Maybe GameStatus)
                :<|> "pass"
                  :> Put '[JSON] (Maybe GameStatus)

                :<|> "proposeTerritory" :> ReqBody '[JSON] Territory
                                        :> Put '[JSON] (Maybe GameStatus)

                :<|> "acceptTerritoryProposal" :> ReqBody '[JSON] Bool
                                               :> Put '[JSON] (Maybe GameStatus)

                :<|> "placeStone" :> ReqBody '[JSON] Position
                                  :> Put '[JSON] (Either MoveError Outcome,Game))


unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts = createNewUser
                 :<|> checkCreds cs jwts
                 :<|> getGamesForPlayer
                 :<|> getGameId

protected :: Servant.Auth.Server.AuthResult UserInput.User -> Server GameAPI
protected (Servant.Auth.Server.Authenticated user) =
  proposeGame user  :<|>
  gameOperations user

protected Servant.Auth.Server.BadPassword = throwAll err401
protected Servant.Auth.Server.NoSuchUser = throwAll err410
protected Servant.Auth.Server.Indefinite = throwAll err406

gameOperations user gameId =
  acceptGameProposal user gameId :<|>
  proposePass user gameId :<|>
  proposeTerritory user gameId :<|>
  acceptTerritoryProposal user gameId :<|>
  placeStone user gameId


type API auths = (Servant.Auth.Server.Auth auths UserInput.User :> GameAPI) :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cookieSettings jwtSettings = protected :<|> unprotected cookieSettings jwtSettings

--TODO: Change this to production settings.
cookieSettings :: CookieSettings
cookieSettings = CookieSettings
    { cookieIsSecure    = NotSecure
    , cookieMaxAge      = Nothing
    , cookieExpires     = Nothing
    , cookiePath        = Just "/"
    , cookieSameSite    = AnySite
    , sessionCookieName = "JWT-Cookie"
    , cookieXsrfSetting = Nothing
    }


main :: IO ()
main = do
  --TODO: Persist this key somewhere (DB?)
  signingKey <- generateKey
  let jwtCfg = defaultJWTSettings signingKey
      cfg = cookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT,Cookie])
  run 8888 $ serveWithContext api cfg (server cookieSettings jwtCfg)

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
    Just (User _ email name _) -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (UserInput.User email name "")
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> pure $ applyCookies NoContent
