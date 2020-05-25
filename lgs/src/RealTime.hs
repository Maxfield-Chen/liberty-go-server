{-# LANGUAGE OverloadedStrings #-}

module RealTime where


import           Data.Text                      (Text)
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.WebSockets
import qualified Network.WebSockets             as WS
import qualified PubSub                         as PB
import           Servant

realTimeApp = websocketsOr WS.defaultConnectionOptions application

application :: WS.PendingConnection -> IO ()
application pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  pure ()
