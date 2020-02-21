{-# language FlexibleContexts #-}

module SHLUG.Seia.Network.MQTT ( mqttNew
                               , mqttLoopbackTest
                               , MQTTConf(..)
                               , MQTT(..)
                               , MQTTState(..)
                               ) where

import SHLUG.Seia.Type
import SHLUG.Seia.Rt (consoleLog)

import qualified System.IO as IO

import Language.Javascript.JSaddle( JSM(..)
                                  , JSVal(..)
                                  , ToJSVal, toJSVal
                                  , FromJSVal, fromJSVal, fromJSValUnchecked
                                  , MonadJSM(..)
                                  , liftJSM
                                  , toJSString
                                  , isNull
                                  , val, fun
                                  , js, jss, jsf
                                  , js0, js1, js2, jsg
                                  , new, obj
                                  , (<#)
                                  , ghcjsPure
                                  , maybeNullOrUndefined
                                  , strToText, textToStr
                                  , valToNumber, valIsNull, valToStr
                                  , valToText
                                  )

import Data.Text ( Text(..) )
import qualified Data.Text as T

import Data.ByteString ( ByteString(..) )
import qualified Data.ByteString.UTF8 as UTF8

import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Concurrent (threadDelay, forkIO)

import Control.Monad (when)

import Reflex
import Control.Lens ((^.))


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
topicPrefix = T.pack "seia的频道_"

topicGen :: NID -> Text
topicGen nid = topicPrefix <> T.pack "/" <> T.pack (show nid)

clientNew :: NID -> Text -> (MQTTState -> IO ()) ->
             (ByteString -> IO ()) -> JSM JSVal
clientNew nid url stT rxT = do
  let topic = topicGen nid
  liftIO $ stT MQTTConnecting
  rt <- jsg "_rt"
  cli <- rt ^. js "mqtt"  ^. js1 "connect" url
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
  -- message
  cli ^. js2 "on" "message" (fun $ \_ _ (_ : m : _) -> do
         --consoleLog "message"
         t <- valToText m
         let x = reads $ T.unpack t
         case x of
           []        -> liftIO $ putStrLn "warn, get malformed mqtt msg"
           (y, _): _ -> liftIO $ rxT y)

  return cli

clientSend :: JSVal -> NID -> ByteString -> JSM ()
clientSend cli nid msg = do
  let topic = topicGen nid
  let payload = show msg

  cli ^. js2 "publish" topic payload
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
