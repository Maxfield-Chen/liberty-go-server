{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module RealTime where


import           Data.Text                      (Text)
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.WebSockets
import qualified Network.WebSockets             as WS
import           Servant

realTimeApp = websocketsOr WS.defaultConnectionOptions wsApp
  where
    wsApp :: WS.ServerApp
    wsApp pending_conn = do
        conn <- WS.acceptRequest pending_conn
        WS.sendTextData conn ("Hello, client!" :: Text)
