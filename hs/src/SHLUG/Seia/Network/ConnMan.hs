{-# language FlexibleContexts #-}
{-# language BlockArguments #-}
{-# language TupleSections #-}

module SHLUG.Seia.Network.ConnMan ( connManNew
                                  , ConnMan(..)
                                  , ConnManConf(..)
                                  ) where
import SHLUG.Seia.Type
import SHLUG.Seia.Msg
import SHLUG.Seia.Network.Conn
import SHLUG.Seia.Network.MQTT
import SHLUG.Seia.Conf
import SHLUG.Seia.Rt
import SHLUG.Seia.Helper

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

data RouteEntry = MkRouteEntry { _re_ts :: Word64
                               , _re_nid :: [NID]
                               , _re_ts' :: Word64
                               , _re_nid' :: [NID]
                               } deriving (Eq, Show)

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
  MkConnManConf { _conn_man_tx :: Event t ByteString
                , _rt_conf :: RtConf
                , _conf :: Behavior t Conf
                }


  -- map (ogm_neig, src, epoch)
rtblUp :: Map NID RouteEntry -> (NID, NID, Word64) -> Map NID RouteEntry
rtblUp m (rid, src, epoch) = snd $ (flip runState) m $
  case Map.lookup src m of
  Nothing -> modify $
             Map.insert src
                        MkRouteEntry
                        { _re_ts = epoch
                        , _re_nid = [rid]
                        , _re_ts' = 0
                        , _re_nid' = []
                        }
  Just re -> if epoch > _re_ts re
             then modify $
                  Map.insert src $
                  re { _re_ts  = epoch     , _re_nid  = [rid]
                     , _re_ts' = _re_ts re , _re_nid' = _re_nid re
                     }
             else if epoch == _re_ts re
                  then modify $
                       Map.insert src $
                       re { _re_nid = let l = _re_nid re in
                                      if elem rid l then l else l ++ [rid]
                          }
                  else return ()

rtblNextConn :: Map NID RouteEntry -> NID -> Map NID Conn -> Maybe (NID, Conn)
rtblNextConn rtbl dst cmap =
  case Map.lookup dst cmap of -- check local connected node
    Just conn -> Just (dst, conn)
    Nothing ->
      case Map.lookup dst rtbl of
      Nothing -> Nothing
      Just re -> let l = _re_nid re ++ _re_nid' re
                     n = L.find (flip Map.member cmap) l
                 in case n of
                    Nothing -> Nothing
                    Just n -> (n,) <$> Map.lookup n cmap

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
  let main_ver = _rt_main_version rconf

  mqtt <- mqttNew $ MkMQTTConf { _mqtt_url = T.pack "wss://test.mosquitto.org:8081"
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
  stD <- accumDyn (\m (nid, st) -> if (st == ConnTimeout) || (st == ConnFail)
                                   then Map.delete nid m
                                   else Map.insert nid st m
                  ) Map.empty stE

  ------ current route table
  (rtblE, rtblT) <- newTriggerEvent
  rtblB <- accumB rtblUp Map.empty rtblE

  -- set turn server & set bootstrap node
  (set_ts_E, set_ts_T) <- newTriggerEvent
  (set_bn_E, set_bn_T) <- newTriggerEvent

  (conn_cb_E, conn_cb_T) <- newTriggerEvent

  conn_cb_B <- accumB (\m (nid, conn') -> case conn' of
                                          Just conn -> Map.insert nid conn m
                                          Nothing -> Map.delete nid m
                      ) Map.empty conn_cb_E

  performEvent_ $ ffor stE $ \(nid, st) ->
    when ((st == ConnTimeout) || (st == ConnFail)) $
         liftIO $ conn_cb_T (nid, Nothing)

  -- TODO, filter from rxPreE, only left MsgSigned & dst is self
  let rxE = never

  -- utils
  let conn_msg_sign = msgSign (_conf_priv_key conf)


  ------------------ mqtt rx
  performEvent_ $ ffor mqtt_rxE $ \raw -> do
    runMaybeT $ do
           when (not $ msgIsRTC raw) $
             liftIO (printf "MQTT: not RTC msg, drop\n") >>
             fail "not rtc msg"
           when (not $ msgVerify raw) $
             liftIO (printf "MQTT: msg verify fail, drop\n") >>
             fail "verify fail"

           liftIO $ rxPreT (nid0, raw)
    return ()

  ----------- monitor
  tick5 <- liftIO getCurrentTime >>= tickLossy 5
  performEvent_ $ ffor tick5 $ \_ -> do
    st <- sample $ current stD
    liftIO $ printf "st -> %s\n" (show st)
    cb <- sample conn_cb_B
    liftIO $ printf "conn_cb -> %s\n" (show $ Map.keys cb)

  ---------------------------- route
  let route dst raw = do
        liftIO $ printf "  route msg to %s\n" (show dst)

        rtbl <- sample rtblB
        mqttSt <- sample $ current mqtt_stateD

        cmap <- sample conn_cb_B

        let conn' = rtblNextConn rtbl dst cmap

        if isJust conn'
        then do
          let (n, conn) = fromJust conn'
          liftIO $ printf "    route exist, via %s\n" (show n)
          liftJSM $ (_conn_tx_cb conn) raw
        else if (mqttSt == MQTTOnline) && msgIsRTC raw
             then do
               liftIO $ printf "    MQTT is online, route RTC message via MQTT\n"
               liftIO $ mqtt_txT (dst, raw)
             else do
               liftIO $ printf "    no route, can not relay via MQTT, drop\n"

        return ()

  -- m -> JSM
  let routeOGM src ogm = do
        let hop = _msg_hop ogm
        let ogm' = ogm { _msg_hop = hop + 1}
        let raw = toStrict $ encode ogm'

        cbMap <- sample conn_cb_B

        liftJSM $ sequence_ $
          Map.mapWithKey (\k v -> when (k /= src) (_conn_tx_cb v $ raw))
                         cbMap

  -- GOM gen
  tick20 <- liftIO getCurrentTime >>= tickLossy 20
  performEvent_ $ ffor tick20 $ \_ -> do
    let ogm = MsgOGM { _msg_type = '\2'
                     , _msg_src = nid
                     , _msg_epoch = 0
                     , _msg_sign = emptySign
                     , _msg_hop = 0
                     }
    ogm1 <- liftIO $ msgFillEpoch ogm
    let raw = toStrict $ encode ogm1
    let rawS = conn_msg_sign raw

    liftIO $ rxPreT (nid, rawS)

  performEvent_ $ ffor rxPreE $ \(rid, m) -> do
    let msg = decode (fromStrict m) :: Msg
    liftIO $ printf "    process rxPre: %s\n" (sss 50 msg)
    stMap <- sample $ current stD
    ts <- _conf_turn_server <$> sample (_conf c)
    rtbl <- sample rtblB

    when (msgIsOGM m) $ do
      let ogm = decode $ fromStrict m
      let hop = _msg_hop ogm
      let src = _msg_src ogm

      liftIO $ printf "  OGM from %s ..., src %s ...\n"
                      (take 7 $ show rid)
                      (take 7 $ show (_msg_src ogm))

      -- update route event
      liftIO $ rtblT (rid, _msg_src ogm, _msg_epoch ogm)
      let newRecord = case Map.lookup src rtbl of
                      Nothing -> True
                      Just re -> _msg_epoch ogm > _re_ts re

      when ((hop < 4) && newRecord) $ routeOGM rid ogm

      return ()

    when (msgIsGeneral m) $ do
      let dst = _msg_dst msg
      when (dst /= nid) $ route dst m

    when (msgIsRTC m) $ do
      let dst = _msg_dst msg
      let src = _msg_src msg
      let st = Map.lookup src stMap

      if (dst /= nid) then route dst m
      else if ( (st /= Nothing) &&
                (st /= Just ConnTimeout) &&
                (st /= Just ConnFail) )
           then do m <- sample conn_cb_B
                   case Map.lookup src m of
                     Just conn -> liftJSM $ (_conn_rtc_rx_cb conn)
                                            ( _msg_epoch msg
                                            , _msg_payload msg)
                     Nothing -> liftIO $ printf "rtc msg dst not exist, drop\n"

           else do let raw = _msg_payload msg
                   let on_conn_st st = liftIO $ stT (src, st)

                   let cc = MkConnConf
                            { _conn_local = nid
                            , _conn_remote = src
                            , _conn_main_ver = main_ver
                            , _conn_type = ConnIsServer
                            , _conn_st_cb = on_conn_st
                            , _conn_rx_cb = liftIO . rxPreT . (src,)
                            , _conn_turn_server = ts
                            , _conn_msg_sign = conn_msg_sign
                            , _conn_nid_exist = Map.member src rtbl -- req node exist?
                            }
                   -- should only create new Conn for MsgRTCReq
                   when (connIsReq raw) $
                     do conn <- connNew cc
                        liftIO $ conn_cb_T (src, Just conn)
                        liftJSM $ (_conn_rtc_rx_cb conn)
                                  (_msg_epoch msg, raw)
                        return ()

  -- tx related
  let txE = _conn_man_tx c
  performEvent_ $ ffor txE $ \raw -> when (msgIsGeneral raw) $
                                       liftIO (rxPreT (nid, raw))


  ----------------- tick
  tick1 <- liftIO getCurrentTime >>= tickLossy 1
  performEvent_ $ ffor tick1  $ \_ -> do
    liftIO $ printf "======= tick =======\n"
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
        liftIO $ putStrLn "  not enough bootstrap candidate"
        fail "not enough bootstrap candidate"

      let dst = head nl' -- TODO, here just select first node
      when ((mqtt_state /= MQTTOnline) &&
            (not $ Map.member dst rtbl)) $ do
                 liftIO $ putStrLn "mqtt offline or not routable, skip"
                 fail "mqtt offline or not routable"

      liftIO $ printf "try to connect to %s\n" (show dst)

      return dst



    when (isJust dst') $ do
      let dst = fromJust dst'
      let on_conn_st x = liftIO $ stT (dst, x)
      ts <- _conf_turn_server <$> sample (_conf c)
      conn <- connNew MkConnConf { _conn_remote = dst
                                 , _conn_local = nid
                                 , _conn_main_ver = main_ver
                                 , _conn_type = ConnIsClient
                                 , _conn_st_cb = on_conn_st
                                 , _conn_rx_cb = liftIO . rxPreT . (dst,)
                                 , _conn_turn_server = ts
                                 , _conn_msg_sign = conn_msg_sign
                                 , _conn_nid_exist = False -- always F for client
                                 }
      liftIO $ conn_cb_T (dst, Just conn)

    return ()
  ---------------------- trace stE change
  performEvent_ $ ffor stE $ \(n, s) ->
    liftIO $ printf "node %s: %s\n" (show n) (show s)





  return MkConnMan { _conn_man_rx = rxE
                   , _conn_man_st_d = stD
                   , _conn_man_st_e = stE
                   , _conn_man_route_table = rtblB
                   , _conn_man_mqtt_state = mqtt_stateD
                   , _conf_set_turn_server = set_ts_E
                   , _conf_set_bootstrap_node = set_bn_E
                   }