{-# language FlexibleContexts #-}
{-# language BlockArguments #-}
{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}

module SHLUG.Seia.Network.ConnMan ( connManNew
                                  , ConnMan(..)
                                  , ConnManConf(..)
                                  ) where
import SHLUG.Seia.Type
import SHLUG.Seia.Msg
import SHLUG.Seia.Msg.Payload
import SHLUG.Seia.Msg.Envelope

import SHLUG.Seia.Network.Conn
import SHLUG.Seia.Network.Route
import SHLUG.Seia.Network.MQTT
import SHLUG.Seia.Conf
import SHLUG.Seia.Rt
import SHLUG.Seia.Helper

import System.Random (randomRIO)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Text (Text(..))
import qualified Data.Text as T

import Data.Binary
import Data.Time (getCurrentTime)
import qualified Data.List as L

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad (when, forever)
import Control.Concurrent (threadDelay, forkIO)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Control.Monad.State (runState, put, get, modify)

import Control.Monad.Fix (MonadFix(..))

import Data.Maybe (isJust, fromJust)

import Text.Printf
import qualified System.IO as IO

import Reflex
import Language.Javascript.JSaddle ( JSM(..), MonadJSM(..)
                                   , liftJSM
                                   )

{-
ConnMan

when recv message from MQTT
1. verify message, then check message, if type is RTC and dst is current node
   then relay to _man_conn_rx_pre, _man_conn_rx_pre is TriggerEvent


when recv message from Conn (_conn_rx of Conn)
1. relay to _conn_man_rx_pre

process mesagge for _conn_man_rx_pre
1. if is OGM, special handle, update route table
2. if is RTC Message,
   - if _msg_dst is current node,
     decode message, extract epoch & payload,
     + if _msg_src is not inside _conn_man_st_d, create a new conn
       insert this message to _conn_rtc_tx of new created conn
     + else relay to the Conn which match _msg_src
   - if _dst is not current node, goto 4
3. if is Signed message,
   - if _msg_dst is currnet node, update _conn_man_rx event
   - if _msg_dst is not currnet, goto 4
4. route message
   - is _dst is in route table, relay to _conn_man_tx_post
   - is not in route table, and is rtc message, and mqtt is up, then send via mqtt

when event in _conn_man_tx
1. drop any non MsgSign2 type message
2. relay to _conn_man_rx_pre

when startup
1. start mqtt
2. check connection state periodically, if online node number < X, try to bootstrap
   1. select a node, from bootstrap table first, then from route table
   2. if mqtt is online or route to this node exist, try connect to this node
   3. or goto 1, select another node
3. update bootstrap list periodically
   1. collect current online direct connected node, and is in server mode
   2. sort by online time,
   3. update to bootstrap list
-}


data ConnMan t = MkConnMan { _conn_man_rx :: Event t (NID, ByteString)
                           , _conn_man_st_d :: Dynamic t (Map NID ConnState)
                           , _conn_man_st_e :: Event t (NID, ConnState)
                           , _conn_man_route_table :: Behavior t (Map NID RouteEntry)
                           , _conn_man_mqtt_state :: Dynamic t MQTTState
                           , _conf_set_turn_server :: Event t [Text]
                           , _conf_set_bootstrap_node :: Event t [Text]
                           }

data ConnManConf t =
  MkConnManConf { _conn_man_tx :: Event t (NID, Payload)
                , _rt_conf :: RtConf
                , _conf :: Behavior t Conf
                }


connManNew :: ( Reflex t
              , TriggerEvent t m
              , MonadSample t m
              , PerformEvent t m
              , MonadFix m
              , MonadHold t m
              , MonadIO m
              , MonadSample t (Performable m)
              , MonadHold t (Performable m)
              , MonadJSM (Performable m)
              , PostBuild t m
              ) =>
              ConnManConf t -> m (ConnMan t)
connManNew c = do
  -- mqtt
  (mqtt_txE, mqtt_txT) <- newTriggerEvent

  conf <- sample (_conf c)
  let nid = _conf_nid conf

  rconf <- liftJSM rtConf
  let main_ver = fst $ _rt_main_version rconf

  --"wss://test.mosquitto.org:8081"
  mqtt <- mqttNew $ MkMQTTConf { _mqtt_url = _rt_mqtt_server rconf
                               , _mqtt_nid = nid
                               , _mqtt_tx = mqtt_txE
                               }

  let mqtt_rxE = _mqtt_rx mqtt
  let mqtt_stateD = _mqtt_state mqtt

  ------------------------ event & behavior declare

  -- msg collect from Conn or MQTT, verified
  (rxPreE, rxPreT) <- newTriggerEvent
  -- current state of Conn
  (stE, stT) <- newTriggerEvent
  let stAccF m (nid, x) = case x of
                          Left conn ->
                            Map.insert nid (conn, ConnIdle) m
                          Right st ->
                            if connStEnd st then Map.delete nid m
                            else Map.update (\(c,_) -> Just (c,st)) nid m

  stD <- accumDyn stAccF Map.empty stE

  ------ current route table

  -- set turn server & set bootstrap node
  (set_ts_E, set_ts_T) <- newTriggerEvent
  (set_bn_E, set_bn_T) <- newTriggerEvent



  -- TODO, filter from rxPreE, only left MsgSigned & dst is self
  let rxE = never

  -- utils
  let conn_msg_sign = msgSign (_conf_priv_key conf)


  ------------------ mqtt rx
  performEvent_ $ ffor mqtt_rxE $ \raw ->
    case decodeOrFail (fromStrict raw) of
      Left _ -> return ()
      Right (_, _, msg @ MsgSigned { _msg_payload = MkRTCMsg _ }) -> do
           when (msgVerify raw) $ liftIO $ rxPreT (nid0, msg, raw)
      _ -> return ()

  ---------------------------- route
  (rxRouteE, rxRouteT) <- newTriggerEvent
  (rtblB, route) <- routeSetup nid conn_msg_sign
                               mqtt_txT mqtt_stateD stE stD rxRouteE

  performEvent_ $ ffor rxPreE $ \(rid, msg, raw) -> do
    liftIO $ printf "    process rxPre: %s\n" (sss 50 msg)
    connStMap <- sample $ current stD
    ts <- _conf_turn_server <$> sample (_conf c)
    rtbl <- sample rtblB

    case msg of
      MsgEnvelopedSignal { _msg_envelope = EvpOGM {} } ->
        liftIO $ rxRouteT (rid, msg)
      -- route message
      MsgSigned { _msg_payload = MkRouteMsg _ } ->
        liftIO $ rxRouteT (rid, msg)

      MsgSigned { _msg_dst = dst } | dst /= nid ->
        route msg raw

      -- rtc message, and dst should == nid
      MsgSigned { _msg_payload = MkRTCMsg rmsg } -> do
        let dst = _msg_dst msg
        let src = _msg_src msg
        let st = snd <$> Map.lookup src connStMap
        when (dst /= nid) $ fail "should sent to self"

        if ( (st /= Nothing) &&
             (st /= Just ConnTimeout) &&
             (st /= Just ConnFail) )
        then do m <- Map.map fst <$> sample (current stD)
                case Map.lookup src m of
                  Just conn -> liftJSM $ (_conn_rtc_rx_cb conn)
                                         (_msg_epoch msg, rmsg)
                  Nothing -> do liftIO $ printf "rtc msg dst not exist, drop\n"
                                return ()

        else do let on_conn_st st = liftIO $ stT (src, Right st)
                let cc = MkConnConf
                         { _conn_local = nid
                         , _conn_remote = src
                         , _conn_main_ver = main_ver
                         , _conn_type = ConnIsServer
                         , _conn_st_cb = on_conn_st
                         , _conn_rx_cb = liftIO . rxPreT . uncurry (src,,)
                         , _conn_turn_server = ts
                         , _conn_msg_sign = conn_msg_sign
                         , _conn_nid_exist = Map.member src rtbl -- req node exist?
                         }
                -- should only create new Conn for MsgRTCReq
                case rmsg of
                  MkRTCReq _ -> do conn <- connNew cc
                                   liftIO $ stT (src, Left conn)
                                   liftJSM $ (_conn_rtc_rx_cb conn)
                                             (_msg_epoch msg, rmsg)
                                   return ()
                  _ -> return ()
      -- other signed message
      _ -> return ()

  -- tx related
  let txE = _conn_man_tx c
  performEvent_ $ ffor txE $ \(rid, pl) -> do
    let msg = MsgSigned { _msg_src = nid
                        , _msg_dst = rid
                        , _msg_epoch = 0
                        , _msg_payload = pl
                        , _msg_sign = emptySign
                        }
    msg1 <- liftIO $ msgFillEpoch msg
    let raw = conn_msg_sign $ toStrict $ encode msg1
    let msg2 = msg1 { _msg_sign = msgGetSignature raw }

    liftIO $ rxPreT (nid, msg2, raw)


  ----------------- tick
  tick1 <- liftIO getCurrentTime >>= tickLossy 1
  performEvent_ $ ffor tick1  $ \_ -> do
    --liftIO $ printf "======= tick =======\n"
    liftIO $ IO.hFlush IO.stdout

  -- automake conn check
  tick1s5 <- liftIO getCurrentTime >>= tickLossy 1.5
  performEvent_ $ ffor tick1s5  $ \_ -> do
    st <- sample $ current stD
    nl <- sample $ _conf_bootstrap_node <$> _conf c
    mqtt_state <- sample $ current mqtt_stateD
    rtbl <- sample rtblB

    dst' <- runMaybeT $ do
      let ff x = (x /= nid) &&
                 (Map.notMember x st)

      let nl' = filter ff nl

      when (length nl' == 0) $ do
        --liftIO $ putStrLn "  not enough bootstrap candidate"
        fail "not enough bootstrap candidate"

      idx <- liftIO $ randomRIO (0, length nl' - 1)
      let dst = nl' !! idx -- TODO
      when ((mqtt_state /= MQTTOnline) &&
            (not $ Map.member dst rtbl)) $ do
                 liftIO $ putStrLn "mqtt offline or not routable, skip"
                 fail "mqtt offline or not routable"

      liftIO $ printf "try to connect to %s\n" (show dst)

      return dst


    when (isJust dst') $ do
      let dst = fromJust dst'
      let on_conn_st x = liftIO $ stT (dst, Right x)
      ts <- _conf_turn_server <$> sample (_conf c)
      conn <- connNew MkConnConf { _conn_remote = dst
                                 , _conn_local = nid
                                 , _conn_main_ver = main_ver
                                 , _conn_type = ConnIsClient
                                 , _conn_st_cb = on_conn_st
                                 , _conn_rx_cb = liftIO . rxPreT . uncurry (dst,,)
                                 , _conn_turn_server = ts
                                 , _conn_msg_sign = conn_msg_sign
                                 , _conn_nid_exist = False -- always F for client
                                 }
      liftIO $ stT (dst, Left conn)

    return ()
  ---------------------- trace stE change
  performEvent_ $ ffor stE $ \(n, x) ->
    let str = case x of { Left _ -> "NEW"; Right st -> show st } in
    liftIO $ printf "node %s: %s\n" (show n) str

  performEvent_ $ ffor (updated stD) $ \st -> do
    liftIO $ printf "st -> %s\n" (show $ Map.map snd st)

  tick30 <- liftIO getCurrentTime >>= tickLossy 30
  performEvent_ $ ffor tick30 $ \_ -> do
    st <- sample $ current stD
    liftIO $ printf "st -> %s\n" (show $ Map.map snd st)
    rtbl <- sample rtblB
    liftIO $ printf "rtbl -> %s\n" (show rtbl)

  ----- output
  let st_d = Map.map snd <$> stD
  let st_e = ffor stE $
             \(nid, x) ->
             (nid, case x of { Left _ -> ConnIdle; Right st -> st})

  return MkConnMan { _conn_man_rx = rxE
                   , _conn_man_st_d = st_d
                   , _conn_man_st_e = st_e
                   , _conn_man_route_table = rtblB
                   , _conn_man_mqtt_state = mqtt_stateD
                   , _conf_set_turn_server = set_ts_E
                   , _conf_set_bootstrap_node = set_bn_E
                   }
