{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module RealTime where


import           Config
import           Control.Concurrent.Async       (race_)
import           Control.Concurrent.STM
import           Control.Exception              (finally)
import           Control.Monad                  (forM_, forever, guard)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Monoid                    (mappend)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS
import qualified PubSub                         as PS
import           PubSubTypes
import           Servant

type RealTimeApp = ReaderT Config IO

racePubSub :: Config -> RealTimeApp a -> RealTimeApp b -> IO ()
racePubSub cfg l r = (runReaderT l cfg) `race_` (runReaderT r cfg)

realTimeApp cfg = websocketsOr WS.defaultConnectionOptions $ application cfg

application :: Config -> WS.PendingConnection -> IO ()
application cfg pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  authResult <- runMaybeT $ extractUserInfo msg
  case authResult of
    Nothing -> WS.sendTextData conn $
      InitialConnectionError "Authorization Denied"
    Just uName -> do
      client <- liftIO . atomically $ PS.makeNewClient uName
      racePubSub cfg
        (receiveLoop client conn)
        (sendLoop client conn)
        `finally` disconnect client

hoistMaybeT :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybeT = MaybeT . pure

-- TODO: Implement validation + claim extraction
extractUserInfo :: Text -> MaybeT IO Text
extractUserInfo _ = hoistMaybeT Nothing
-- extractUserInfo d = do
--   let token = Token d
--   claim <- hoistMaybeT $ getClaimSetFromToken token
--   isValid <- liftIO $ checkExpValid claim
--   case isValid of
--     True -> do
--       uid <- hoistMaybeT $ iss claim
--       pure $ stringOrURIToText uid
--     False -> hoistMaybeT Nothing

receive :: WS.Connection -> RealTimeApp (Either ErrorMessage IncomingMessage)
receive = liftIO . WS.receiveData

send :: (WS.WebSocketsData a) => WS.Connection -> a -> RealTimeApp ()
send conn = liftIO . WS.sendTextData conn

receiveLoop :: Client -> WS.Connection -> RealTimeApp ()
receiveLoop client conn = do
  gmap <- asks gameMap
  forever $ do
    recMessage <- receive conn
    case recMessage of
      Left err -> send conn err
      Right (JoinGame g) ->
        liftIO . atomically $ do
          game <- PS.getGame g gmap
          subscribed <- PS.clientSubbed client game
          guard (not subscribed)
          PS.subscribe client game
      Right (LeaveGame g) ->
        liftIO . atomically $ do
          game <- PS.getGame g gmap
          subscribed <- PS.clientSubbed client game
          guard subscribed
          PS.leave client game

sendLoop :: Client -> WS.Connection -> RealTimeApp ()
sendLoop client conn = forever $ do
  (liftIO . atomically $ PS.getAvailableMessage client) >>= send conn

disconnect :: Client -> IO ()
disconnect client@Client{..} = atomically $ do
  gameSet <- readTVar clientGames
  forM_ gameSet (PS.leave client)
