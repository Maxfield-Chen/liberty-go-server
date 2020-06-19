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

import           Config
import           Control.Monad.Except                (liftIO)
import           Control.Monad.Reader
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Text                           (Text)
import qualified Game                                as G
import qualified GameApiImpl                         as GI
import qualified GameDB                              as GDB
import qualified GameExpressions                     as GEX
import qualified LGSAPI                              as C
import           Network.Wai.Handler.Warp            (run)
import qualified PubSubTypes                         as PST
import qualified RealTime                            as RT
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import qualified UserInput

unprotected :: CookieSettings -> JWTSettings -> ServerT C.Unprotected GI.AppM
unprotected cs jwts = GI.createNewUser
                 :<|> checkCreds cs jwts
                 :<|> GI.getGamesForPlayer
                 :<|> GI.getGameId

protected :: Servant.Auth.Server.AuthResult UserInput.User -> ServerT C.GameAPI GI.AppM
protected (Servant.Auth.Server.Authenticated user) =
  GI.proposeGame user  :<|>
  GI.getGamesForProfile user :<|>
  GI.getUserId user :<|>
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

server :: CookieSettings -> JWTSettings -> ServerT (C.API auths) GI.AppM
server cookieSettings jwtSettings =
  protected :<|>
  unprotected cookieSettings jwtSettings

--TODO: Change this to production settings.

mkApp :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> Config
      -> Application
mkApp ctx cs jwtCfg cfg =
  let api = Proxy :: Proxy (C.API '[JWT,Cookie])
  in serveWithContext api ctx $
    hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings])
      (flip runReaderT cfg) (server cs jwtCfg)

main :: IO ()
main = do
  --TODO: Persist this key somewhere (DB?)
  signingKey <- generateKey
  initialGameMap <- PST.emptyGameMap
  let jwtCfg = defaultJWTSettings signingKey
      cfg = defaultConfig {jwtSecret = signingKey
                         , gameMap = initialGameMap}
      cs = cookieSettings cfg
      ctx = cs :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (C.API '[JWT,Cookie])
  run 8888 $ RT.realTimeApp cfg $ mkApp ctx cs jwtCfg cfg

--TODO: Hash password before lookup
checkCreds :: CookieSettings
           -> JWTSettings
           -> UserInput.Login
           -> GI.AppM (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                                NoContent)
checkCreds cookieSettings jwtSettings (UserInput.Login name pass) = do
  mUser <- liftIO $ GEX.getUserViaCreds name pass
  case mUser of
    Nothing -> throwError err401
    Just (GDB.User id email name _) -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (UserInput.User email name id)
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> pure $ applyCookies NoContent
