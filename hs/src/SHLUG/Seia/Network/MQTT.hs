{-# language FlexibleContexts #-}

module SHLUG.Seia.Network.MQTT ( mqttNew
                               , mqttLoopbackTest
                               , MQTTConf(..)
                               , MQTT(..)
                               , MQTTState(..)
                               ) where

import SHLUG.Seia.Type
import SHLUG.Seia.Rt (consoleLog, js_rt)
import SHLUG.Seia.Conf(confConst, ConfConst(..))

import qualified System.IO as IO

import Language.Javascript.JSaddle( JSM(..)
                                  , JSVal(..)
                                  , ToJSVal, toJSVal
                                  , FromJSVal, fromJSVal, fromJSValUnchecked
                                  , MonadJSM(..)
                                  , askJSM, runJSM
                                  , liftJSM
                                  , toJSString
                                  , isNull
                                  , val, fun
                                  , js, jss, jsf
                                  , js0, js1, js2
                                  , new, obj
                                  , (<#)
                                  , ghcjsPure
                                  , catch, JSException(..)
                                  , maybeNullOrUndefined
                                  , strToText, textToStr
                                  , valToNumber, valIsNull, valToStr, valToBool
                                  , valToText
                                  )

import Data.Text ( Text(..) )
import qualified Data.Text as T

import Data.ByteString ( ByteString(..) )
import qualified Data.ByteString.UTF8 as UTF8

import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Concurrent (threadDelay, forkIO)

import Control.Monad (when, forever)

import Reflex
import Control.Lens ((^.))
import Data.Time
import Data.IORef


data MQTTConf t = MkMQTTConf { _mqtt_url :: Text
                             , _mqtt_nid :: NID
                             , _mqtt_tx :: Event t (NID, ByteString)
                           }
data MQTTState = MQTTOffline |
                 MQTTConnecting |
                 MQTTOnline
               deriving (Eq, Show)

data MQTT t = MkMQTT { _mqtt_state :: Dynamic t MQTTState
                     , _mqtt_rx :: Event t ByteString
                     }

topicPrefix :: Text
topicPrefix = T.pack "seia的频道_2"

topicGen :: NID -> Text
topicGen nid = topicPrefix <> T.pack "/" <> T.pack (show nid)

clientNew :: NID -> Text -> (MQTTState -> IO ()) ->
             (ByteString -> IO ()) -> JSM JSVal
clientNew nid url stT rxT = do
  let topic = topicGen nid
  liftIO $ stT MQTTConnecting
  cli <- js_rt ^. js "mqtt"  ^. js1 "connect" url
  -- handle connect
  cli ^. js2 "on" "connect" (fun $ \_ _ _ -> do
    -- subscribe
    cli ^. js2 "subscribe" topic (fun $ \_ _ (err:_) -> do
      ok <- ghcjsPure $ isNull err
      --consoleLog ("subscribe", err)
      when ok $ liftIO $ stT MQTTOnline)
    return ())
  -- offline
  cli ^. js2 "on" "offline"
             (fun $ \_ _ _ -> liftIO $ stT MQTTOffline)
  -- close
  cli ^. js2 "on" "close"
             (fun $ \_ _ _ -> liftIO $ stT MQTTOffline)
  -- reconnect
  cli ^. js2 "on" "reconnect"
             (fun $ \_ _ _ -> liftIO $ stT MQTTConnecting)

  tsRef <- liftIO (getCurrentTime >>= newIORef)
  -- TODO: some times, send msg is ok, but can not recv msg
  -- TODO: after computer wake-up from sleep, MQTT might behavior incorrectly
  -- message
  cli ^. js2 "on" "message" (fun $ \_ _ (_ : m : _) -> do
         --consoleLog "message"
         t <- valToText m
         if (t == T.empty) then liftIO (getCurrentTime >>= atomicWriteIORef tsRef)
         else case reads $ T.unpack t of
                   []        -> liftIO $ putStrLn "warn, get malformed mqtt msg"
                   (y, _): _ -> liftIO $ rxT y)

  ------------- heart beat
  {-

  -- NOTE: below code not work, after timeout call 'reconnect'
  -- client will flip-flop between online & offline
  let ti = _cc_mqtt_heartbeat_interval confConst
  let to = _cc_mqtt_heartbeat_timeout  confConst

  ctx <- askJSM
  -- add heartbeat
  liftIO $ forkIO $ forever $ do
           -- may fail
           irunJSM (cli ^. js2 "publish" topic T.empty) ctx >> return ()
           threadDelay $ ti * 1000 * 1000
           x <- (cli ^. js "connected") >>= valToBool
           when x $ do
                t0 <- readIORef tsRef
                t1 <- getCurrentTime
                let dt = diffUTCTime t1 t0
                when (floor dt > to) $ do
                     runJSM (cli ^. js0 "reconnect") ctx
                     putStrLn "mqtt timeout, reconnect"
                     return ()
  -}
  return cli

clientSend :: JSVal -> NID -> ByteString -> JSM ()
clientSend cli nid msg = do
  let topic = topicGen nid
  let payload = show msg

  catch ((cli ^. js2 "publish" topic payload) >> pure ())
        (\(JSException e) -> return ())
  --consoleLog ("publish", topic, payload)

  return ()

mqttNew :: ( Reflex t
           , MonadJSM m
           , MonadHold t m
           , PerformEvent t m
           , MonadIO (Performable m)
           , TriggerEvent t m
           ) =>
           MQTTConf t -> m (MQTT t)
mqttNew c = do
  (stE, stT) <- newTriggerEvent
  (rxE, rxT) <- newTriggerEvent
  stD <- holdDyn MQTTOffline stE
  cli <- liftJSM $ clientNew (_mqtt_nid c) (_mqtt_url c) stT rxT

  performEvent_ $ ffor (_mqtt_tx c) $
                       \(nid, msg) -> liftJSM $ clientSend cli nid msg

  return MkMQTT { _mqtt_state = stD
                , _mqtt_rx = rxE
                }

mqttLoopbackTest :: ( Reflex t
                    , TriggerEvent t m
                    , PerformEvent t m
                    , MonadIO (Performable m)
                    , MonadHold t m
                    , MonadJSM m) =>
                    m ()
mqttLoopbackTest = do
  (txE, txT) <- newTriggerEvent
  let c = MkMQTTConf { _mqtt_url = T.pack "wss://test.mosquitto.org:8081"
                     , _mqtt_nid = nid0
                     , _mqtt_tx = txE
                     }

  liftIO $ forkIO $ mapM_ (\cnt -> threadDelay (1000 * 1000) >>
                                   putStrLn ("send msg " <> show cnt) >>
                                   IO.hFlush IO.stdout >>
                                   txT (nid0, UTF8.fromString (show cnt))) [0..]

  mqtt <- mqttNew c
  performEvent_ $ ffor (updated $ _mqtt_state mqtt) $ \st ->
    liftIO $ (putStrLn $ "st = " <> show st) >> IO.hFlush IO.stdout
  performEvent_ $ ffor (_mqtt_rx mqtt) $ \m ->
    liftIO $ (putStrLn $ "rx = " <> show m) >> IO.hFlush IO.stdout
