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
import           Data.Aeson
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import qualified Data.CaseInsensitive           as CI
import           Data.Either                    (fromRight)
import           Data.HashMap.Strict            as M hiding (foldr)
import           Data.List
import           Data.Maybe                     (fromJust, fromMaybe)
import           Data.Monoid                    (mappend)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding             as TES
import qualified Game as G
import           Data.Text.Lazy.Encoding        as TEL
import           Debug.Trace
import           Network.HTTP.Types.Status
import  qualified OutputTypes as OT
import           Network.Wai
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS
import qualified PubSub                         as PB
import qualified GameExpressions as GEX
import qualified GameDB as GDB
import qualified GameApiImpl as GI
import           PubSubTypes
import           Servant
import           Web.Cookie

type RealTimeApp = ReaderT Config IO

racePubSub :: Config -> RealTimeApp a -> RealTimeApp b -> IO ()
racePubSub cfg l r = (runReaderT l cfg) `race_` (runReaderT r cfg)

realTimeApp cfg = websocketsOr WS.defaultConnectionOptions $ application cfg

application :: Config -> WS.PendingConnection -> IO ()
application cfg pending = do
  let reqHead = WS.pendingRequest pending
  authResult <- extractClaims (jwtSecret cfg) (getJWTFromHeader $ WS.requestHeaders reqHead)
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  case authResult of
    Left _ -> WS.sendTextData conn $
      InitialConnectionError ""
    Right claimsSet -> do
      let uName =
            claimsSet ^. unregisteredClaims &
              TES.decodeUtf8 . BL.toStrict . encode . M.lookupDefault "invalidTokenUser" "dat"
      client <- liftIO . atomically $ PB.makeNewClient uName
      trace ("New Client" ++ show client) $ racePubSub cfg
        (receiveLoop client conn)
        (sendLoop client conn)
        `finally` disconnect client

-- Search for 'JWT-Cookie' value in cookie header
getJWTFromHeader :: WS.Headers -> Maybe BL.ByteString
getJWTFromHeader headers =
  let mCookies = (parseCookies . snd) <$>
        find (\(k,_) -> (TES.decodeUtf8 (CI.original k)) == "Cookie") headers
  in BL.fromStrict . snd <$> (mCookies >>= find (\(k,v) -> k == "JWT-Cookie"))

extractClaims :: JWK -> Maybe BL.ByteString -> IO (Either JWTError ClaimsSet)
extractClaims _ Nothing = pure $ Left (JWSError (JSONDecodeError "Cookie Header Not Found"))
extractClaims jwkSecret (Just rawJWT) = runExceptT $
  decodeCompact rawJWT >>=
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
      Left err -> trace (show err) $ send conn err
      Right (JoinGame g) ->
        trace ("Joining: " ++ show client) $ liftIO . atomically $ do
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
sendLoop client@Client{..} conn = do
  -- TODO: see if there is a way to avoid from just here.
  user <- fromJust <$> GEX.getUserViaName clientName
  forever $ do
    message <- liftIO . atomically $ PB.getAvailableMessage client
    case message of
      UpdateGame _ -> send conn message
      ChatMessage chatMessage@OT.ChatMessage{..} -> do
        userType <- GEX.getUserType (GDB._userId user) chatMessageGameId
        mGameRecord <- GEX.getGameRecord chatMessageGameId
        let gameInProgress = fromMaybe True $ (==) G.InProgress . G._status . GDB._game <$> mGameRecord
        guard (OT.shouldShowMessages userType gameInProgress chatMessage)
        send conn message

disconnect :: Client -> IO ()
disconnect client@Client{..} = atomically $ do
  gameSet <- readTVar clientGames
  forM_ gameSet (PB.leave client)
