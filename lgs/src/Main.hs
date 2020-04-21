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
import qualified Game                                as G
import qualified GameApiImpl                         as GI
import qualified GameDB                              as GDB
import qualified GameExpressions                     as GEX
import qualified LGSAPI                              as C
import           Network.Wai.Handler.Warp            (run)
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import qualified UserInput

unprotected :: CookieSettings -> JWTSettings -> Server C.Unprotected
unprotected cs jwts = GI.createNewUser
                 :<|> checkCreds cs jwts
                 :<|> GI.getGamesForPlayer
                 :<|> GI.getGameId

protected :: Servant.Auth.Server.AuthResult UserInput.User -> Server C.GameAPI
protected (Servant.Auth.Server.Authenticated user) =
  GI.proposeGame user  :<|>
  gameOperations user

protected Servant.Auth.Server.BadPassword = throwAll err401
protected Servant.Auth.Server.NoSuchUser = throwAll err410
protected Servant.Auth.Server.Indefinite = throwAll err406

gameOperations user  =
  GI.acceptGameProposal user  :<|>
  GI.proposePass user  :<|>
  GI.proposeTerritory user  :<|>
  GI.acceptTerritoryProposal user  :<|>
  GI.placeStone user

server :: CookieSettings -> JWTSettings -> Server (C.API auths)
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
      api = Proxy :: Proxy (C.API '[JWT,Cookie])
  run 8888 $ serveWithContext api cfg (server cookieSettings jwtCfg)

--TODO: Hash password before lookup
checkCreds :: CookieSettings
           -> JWTSettings
           -> UserInput.Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                                NoContent)
checkCreds cookieSettings jwtSettings (UserInput.Login name pass) = do
  mUser <- liftIO $ GEX.getUserViaCreds name pass
  case mUser of
    Nothing -> throwError err401
    Just (GDB.User _ email name _) -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (UserInput.User email name "")
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> pure $ applyCookies NoContent
