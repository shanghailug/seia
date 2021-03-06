{-# language FlexibleContexts #-}
{-# language BlockArguments #-}
{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}

{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}


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
import SHLUG.Seia.Log

import System.Random (randomRIO)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.Set (Set(..))
import qualified Data.Set as Set

import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Text (Text(..))
import qualified Data.Text as T

import Data.Binary
import Data.Time (getCurrentTime)
import qualified Data.List as L

import Data.Foldable (toList, foldr')

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad (when, forever)
import Control.Concurrent (threadDelay, forkIO)

import Control.Monad.Trans.Except (runExceptT, except)
import Control.Monad.State (runState, put, get, modify)

import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Reader (ask)

import Data.Maybe (isJust, fromJust)
import Data.Either (isRight, fromRight)

import Text.Printf
import qualified System.IO as IO

import Reflex
import Language.Javascript.JSaddle ( JSM(..), MonadJSM(..)
                                   , liftJSM
                                   )

import GHC.Stack ( HasCallStack, CallStack, callStack, getCallStack, popCallStack
                 , withFrozenCallStack)

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
                           , _conn_man_rx' :: Event t NID
                           , _conn_man_rtt :: Event t (NID, Int)
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
              , WithLogIO m
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

  logEnv <- ask
  let logJSM sev msg = logIOM' logEnv sev msg

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

  -- used for last received msg
  (rxE', rxT') <- newTriggerEvent
  (rttE, rttT) <- newTriggerEvent


  ------ current route table

  -- set turn server & set bootstrap node
  (set_ts_E, set_ts_T) <- newTriggerEvent
  (set_bn_E, set_bn_T) <- newTriggerEvent



  -- TODO, filter from rxPreE, only left MsgSigned & dst is self
  let rxE = never

  -- utils
  let conn_msg_sign = msgSign (_conf_priv_key conf)

  -- start conn proc thread
  connInit logJSM conn_msg_sign

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

  ----------------------- rxPre, filter already processed message
  (oldMsg_E, oldMsg_T) <- newTriggerEvent
  oldMsg_B <- accumB (\(set, seq) op ->
              case op of
                Left ep -> let func = (< ep) . snd
                               sigs = map fst $ toList $ Seq.takeWhileL func seq
                               seq' = Seq.dropWhileL func seq
                               set' = foldr' Set.delete set sigs
                               in (set', seq')
                Right (sig, ep) -> (Set.insert sig set, seq |> (sig, ep))
              ) (Set.empty, Seq.empty) oldMsg_E

  -- scan old message
  tick5 <- liftIO getCurrentTime >>= tickLossy 5
  performEvent_ $ ffor tick5 $ \_ -> do
    ms <- liftIO $ getEpochMs
    (set, seq) <- sample oldMsg_B
    --liftIO $ printf "set: %d/%d\n" (Set.size set) (Seq.length seq)
    liftIO $ oldMsg_T $ Left (ms - 5000)


  let saveOldMsg' sig ep = do
        oldMsg_T $ Right (sig, ep)

  let saveOldMsg m ep = do
        case m of
          MsgEnvelopedSignal { _msg_sign = sig } ->
                             saveOldMsg' sig ep

          MsgSigned { _msg_sign = sig } ->
                    saveOldMsg' sig ep

          _ -> return ()

  let rxPreE' = attachWithMaybe (\(set, _) (rid, msg, raw) ->
                case msg of
                -- only filter signed message, enveloped message should special hanale
                MsgSigned { _msg_sign = sig } ->
                  if Set.member sig set then Nothing else Just (rid, msg, raw)
                _ -> Just (rid, msg, raw)
                ) oldMsg_B rxPreE

  performEvent_ $ ffor rxPreE' $ \(rid, msg, raw) -> do
    logIOM D $ T.pack $ printf "process rxPre: %s" (sss 50 msg)
    connStMap <- sample $ current stD
    ts <- _conf_turn_server <$> sample (_conf c)
    rtbl <- sample rtblB

    -- use local time, avoid time mismatche between node cause
    ep <- liftIO $ getEpochMs
    liftIO $ saveOldMsg msg ep

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
                  Nothing -> do logIOM W $ "rtc msg dst not exist, drop"
                                return ()

        else do let on_conn_st st = liftIO $ stT (src, Right st)
                let cc = MkConnConf
                         { _conn_local = nid
                         , _conn_remote = src
                         , _conn_main_ver = main_ver
                         , _conn_type = ConnIsServer
                         , _conn_st_cb = on_conn_st
                         , _conn_rx_cb = liftIO . rxPreT . uncurry (src,,)
                         , _conn_rx_cb' = liftIO $ rxT' src
                         , _conn_rtt_cb = liftIO . rttT . (src,)
                         , _conn_turn_server = ts
                         }
                -- should only create new Conn for MsgRTCReq
                case rmsg of
                  MkRTCReq _ -> do liftIO $ stT (src, Left connDummy)
                                   -- ^ avoid race condition
                                   conn <- connNew cc
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
    logIOM D $ T.pack $ printf "-----------> tick"
    liftIO $ IO.hFlush IO.stdout

  -- automake conn check
  tick1s5 <- liftIO getCurrentTime >>= tickLossy 1.5
  performEvent_ $ ffor tick1s5  $ \_ -> do
    st <- sample $ current stD
    nl <- sample $ _conf_bootstrap_node <$> _conf c
    mqtt_state <- sample $ current mqtt_stateD
    rtbl <- sample rtblB

    dst' <- runExceptT $ do
      let ff x = (x /= nid) &&
                 (Map.notMember x st)

      let nl' = filter ff nl

      when (length nl' == 0) $ do
        except (Left "not enough bootstrap candidate")

      idx <- liftIO $ randomRIO (0, length nl' - 1)
      let dst = nl' !! idx -- TODO
      when ((mqtt_state /= MQTTOnline) &&
            (not $ Map.member dst rtbl)) $
                 except (Left "mqtt offline or not routable, skip")

      return dst

    case dst' of
     Left msg -> logIOM D msg
     Right dst -> do
      logIOM I $ T.pack $ printf "try to connect to %s" (show dst)
      let dst = fromRight nid0 dst'
      let on_conn_st x = liftIO $ stT (dst, Right x)
      ts <- _conf_turn_server <$> sample (_conf c)
      -- avoid race condition
      liftIO $ stT (dst, Left connDummy)
      conn <- connNew MkConnConf { _conn_remote = dst
                                 , _conn_local = nid
                                 , _conn_main_ver = main_ver
                                 , _conn_type = ConnIsClient
                                 , _conn_st_cb = on_conn_st
                                 , _conn_rx_cb = liftIO . rxPreT . uncurry (dst,,)
                                 , _conn_rx_cb' = liftIO $ rxT' dst
                                 , _conn_rtt_cb = liftIO . rttT . (dst,)
                                 , _conn_turn_server = ts
                                 }
      liftIO $ stT (dst, Left conn)

    return ()
  ---------------------- trace stE change
  performEvent_ $ ffor stE $ \(n, x) ->
    let str = case x of { Left _ -> "NEW"; Right st -> show st } in
    logIOM I $ T.pack $ printf "node %s: %s" (show n) str

  performEvent_ $ ffor (updated stD) $ \st -> do
    logIOM D $ T.pack $ printf "st -> %s" (show $ Map.map snd st)

  tick30 <- liftIO getCurrentTime >>= tickLossy 30
  performEvent_ $ ffor tick30 $ \_ -> do
    st <- sample $ current stD
    logIOM D $ T.pack $ printf "st -> %s" (show $ Map.map snd st)
    rtbl <- sample rtblB
    logIOM D $ T.pack $ printf "rtbl -> %s" (show rtbl)

  ----- output
  let st_d = Map.map snd <$> stD
  let st_e = ffor stE $
             \(nid, x) ->
             (nid, case x of { Left _ -> ConnIdle; Right st -> st})

  return MkConnMan { _conn_man_rx = rxE
                   , _conn_man_rx' = rxE'
                   , _conn_man_rtt = rttE
                   , _conn_man_st_d = st_d
                   , _conn_man_st_e = st_e
                   , _conn_man_route_table = rtblB
                   , _conn_man_mqtt_state = mqtt_stateD
                   , _conf_set_turn_server = set_ts_E
                   , _conf_set_bootstrap_node = set_bn_E
                   }
