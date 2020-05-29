{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Concurrent.STM
import           Crypto.JOSE.JWK
import           Data.Aeson
import           Data.HashMap.Strict    as M
import           GHC.Generics
import           PubSubTypes            (GameMap)
import           Servant.Auth.Server

data Environment = Development | Test | Production
    deriving (Eq, Show, Read)

data Config = Config
  { jwtSecret      :: JWK
  , env            :: Environment
  , cookieSettings :: CookieSettings
  , port           :: Int
  , gameMap        :: GameMap} deriving (Generic)

defaultConfig :: Config
defaultConfig = Config
  { jwtSecret = undefined
  , env = Development
  , cookieSettings = devCookieSettings
  , port = 8888
  , gameMap = undefined}

devCookieSettings :: CookieSettings
devCookieSettings = CookieSettings
    { cookieIsSecure    = NotSecure
    , cookieMaxAge      = Nothing
    , cookieExpires     = Nothing
    , cookiePath        = Just "/"
    , cookieSameSite    = AnySite
    , sessionCookieName = "JWT-Cookie"
    , cookieXsrfSetting = Nothing
    }
