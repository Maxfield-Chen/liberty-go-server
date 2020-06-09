{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module RealTime where


import           Config
import           Control.Concurrent.Async       (race_)
import           Control.Concurrent.STM
import           Control.Exception              (finally)
import           Control.Lens
import           Control.Monad                  (forM_, forever, guard)
import           Control.Monad.Except
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Crypto.JOSE.JWK
import           Crypto.JWT
import qualified Data.ByteString.Lazy           as BL
import           Data.Either                    (fromRight)
import           Data.Maybe                     (fromJust)
import           Data.Monoid                    (mappend)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS
import qualified PubSub                         as PB
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
  authResult <- extractClaims (jwtSecret cfg) msg
  case authResult of
    Left _ -> WS.sendTextData conn $
      InitialConnectionError "Authorization Denied"
    Right claimsSet -> do
      -- TODO: fromJust is only for testing here while I determine what part of JWT to use for UUID
      let uName = fromJust $ claimsSet ^. claimJti
      client <- liftIO . atomically $ PB.makeNewClient uName
      racePubSub cfg
        (receiveLoop client conn)
        (sendLoop client conn)
        `finally` disconnect client

extractClaims :: JWK -> Text -> IO (Either JWTError ClaimsSet)
extractClaims jwkSecret rawJWT = runExceptT $ do
  decodeCompact (BL.fromStrict $ encodeUtf8 rawJWT) >>=
    verifyClaims (defaultJWTValidationSettings (const True)) jwkSecret

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
          game <- PB.getGame g gmap
          subscribed <- PB.clientSubbed client game
          guard (not subscribed)
          PB.subscribe client game
      Right (LeaveGame g) ->
        liftIO . atomically $ do
          game <- PB.getGame g gmap
          subscribed <- PB.clientSubbed client game
          guard subscribed
          PB.leave client game

sendLoop :: Client -> WS.Connection -> RealTimeApp ()
sendLoop client conn = forever $ do
  (liftIO . atomically $ PB.getAvailableMessage client) >>= send conn

disconnect :: Client -> IO ()
disconnect client@Client{..} = atomically $ do
  gameSet <- readTVar clientGames
  forM_ gameSet (PB.leave client)
