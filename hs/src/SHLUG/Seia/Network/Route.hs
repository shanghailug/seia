{-# language TupleSections #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language DeriveGeneric, DeriveDataTypeable #-}

module SHLUG.Seia.Network.Route ( RouteEntry(..)
                                , routeSetup
                                ) where


import SHLUG.Seia.Type

import SHLUG.Seia.Msg
import SHLUG.Seia.Msg.Payload
import SHLUG.Seia.Msg.Envelope

import SHLUG.Seia.Network.Conn
import SHLUG.Seia.Network.MQTT
import SHLUG.Seia.Conf
import SHLUG.Seia.Rt
import SHLUG.Seia.Helper

import GHC.Generics

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Text (Text(..))
import qualified Data.Text as T

import Data.Binary
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

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


  -- map (ogm_neig, src, epoch)
rtblUp :: Map NID RouteEntry -> (NID, NID, Word64) -> Map NID RouteEntry
rtblUp m (rid, src, epoch) = snd $ (flip runState) m $ do
  when (epoch > 0) $
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

  -- if epoch == 0, we delete rid. NOTE, this is expensive
  when (epoch == 0) $ modify $ Map.mapMaybe (rtblDelEntry rid)
  -- when epoch == 1, src is down
  when (epoch == 1) $ modify $ Map.delete src

rtblDelEntry :: NID -> RouteEntry -> Maybe RouteEntry
rtblDelEntry x re = let
  nid = L.delete x $ _re_nid re
  nid' = L.delete x $ _re_nid' re
  in if L.null nid && L.null nid'
     then Nothing
     else Just $ re { _re_nid = nid, _re_nid' = nid' }

rtblNextConn :: Map NID RouteEntry -> NID -> Map NID Conn -> Maybe (NID, Conn)
rtblNextConn rtbl dst cmap =
  case Map.lookup dst rtbl of
      Nothing -> Nothing
      Just re -> let l = _re_nid re ++ _re_nid' re
                     n = L.find (flip Map.member cmap) l
                 in case n of
                    Nothing -> Nothing
                    Just n -> (n,) <$> Map.lookup n cmap

routeSetup :: ( Reflex t
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
              NID -> (ByteString -> ByteString) ->
              ((NID, ByteString) -> IO ()) ->
              Dynamic t MQTTState ->
              Event t (NID, Either Conn ConnState) ->
              Dynamic t (Map NID (Conn, ConnState)) ->
              Event t (NID, Msg) ->
              m ( Behavior t (Map NID RouteEntry)
                , Msg -> ByteString -> Performable m ())
routeSetup nid sign mqtt_txT mqtt_stateD stE stD rxMsgE = do

  (rtblE, rtblT) <- newTriggerEvent
  rtblB <- accumB rtblUp Map.empty rtblE

  -- Conn life-cycle monitor for rtbl
  performEvent_ $ ffor stE $ \(nid, x) -> case x of
    Right st | connStEnd st -> liftIO $ rtblT (nid, nid, 0)
    Right (ConnReady _) -> liftIO $ getEpochMs >>= (rtblT . (nid, nid, ))
    _ -> return ()

  -- ensure msg is Signed Message
  let route msg raw = do
        let dst = _msg_dst msg
        liftIO $ printf "  route msg to %s\n" (show dst)

        rtbl <- sample rtblB
        mqttSt <- sample $ current mqtt_stateD

        cmap <- Map.map fst <$> sample (current stD)
        -- now, rtbl will only containing routable node(include neighbor),
        -- rtblNextConn will not check cmap before check rtbl
        let conn' = rtblNextConn rtbl dst cmap
        let msgIsRTC = case msg of
                       MsgSigned { _msg_payload = MkRTCMsg _ } -> True
                       _ -> False

        if isJust conn'
        then do
          let (n, conn) = fromJust conn'
          liftIO $ printf "    route exist, via %s\n" (show n)
          liftJSM $ (_conn_tx_cb conn) raw
        else if (mqttSt == MQTTOnline) && msgIsRTC
             then do
               liftIO $ printf "    MQTT is online, route RTC message via MQTT\n"
               liftIO $ mqtt_txT (dst, raw)
             else do
               liftIO $ printf "    no route, can not relay via MQTT, drop\n"

        return ()

  -- m -> JSM
  let routeOGM src ogm = do
        let evp = _msg_envelope ogm
        let hop = _evp_ogm_hop evp
        let evp' = evp { _evp_ogm_hop = hop + 1}
        let ogm' = ogm { _msg_envelope = evp' }
        let raw = toStrict $ encode ogm'

        connSt <- sample $ current stD
        let cmap = Map.map fst connSt
        let st = Map.map snd connSt

        -- remote not ready neighbor
        let cmap' = Map.filterWithKey (\k _ -> case Map.lookup k st of
                                               Just (ConnReady _) -> True
                                               _ -> False
                                      ) cmap

        liftJSM $ sequence_ $
          Map.mapWithKey (\k v -> when (k /= src) (_conn_tx_cb v $ raw))
                         cmap'


  -- GOM gen
  let genOGM = do
        let ogm = MsgEnvelopedSignal { _msg_src = nid
                                     , _msg_epoch = 0
                                     , _msg_sign = emptySign
                                     , _msg_envelope = EvpOGM 0
                                     }
        ogm1 <- liftIO $ msgFillEpoch ogm
        let raw = sign $ toStrict $ encode ogm1
        return $ ogm1 { _msg_sign = msgGetSignature raw }

  let sendOGM = genOGM >>= routeOGM nid
  -- send OGM periodically
  tickOGM <- liftIO getCurrentTime >>= tickLossy (realToFrac $ _cc_cm_ogm_interval confConst)
  performEvent_ $ ffor tickOGM $ const sendOGM

  ------ rtbl clean up
  let timeoutNodeE = ffor (attach rtblB tickOGM) $ \(rtbl, tick) ->
        Map.keys $ Map.filter (
          \re -> let ts = fromIntegral (_re_ts re) / 1000
                     t0 = posixSecondsToUTCTime ts
                     diff = diffUTCTime (_tickInfo_lastUTC tick) t0
                 in diff > realToFrac (_cc_cm_ogm_timeout confConst)
        ) rtbl

  performEvent_ $ ffor timeoutNodeE $ \n -> do
    liftIO $ mapM_ (\x -> rtblT (x, x, 1)) n
    liftIO $ printf "  timeout node: %s\n" (show n)

  -- TODO, need save recent msg, routead msg will drop

  -- when send OGM to first online neighbor
  performEvent_ $ ffor stE $ \(nid, x) -> case x of
    Right (ConnReady _) -> do
      st <- sample $ current stD
      when (L.null $ L.delete nid (Map.keys st)) sendOGM
    _ -> return ()

  -- send MkRouteInit msg to new client
  performEvent_ $ ffor stE $ \(rid, x) -> case x of
    Right (ConnReady ConnIsServer) -> do
          st <- sample $ current stD
          epoch <- liftIO getEpochMs
          rtbl <- sample rtblB
          case Map.lookup rid st of
               Just (conn, _) ->
                    let rmsg = MkRouteInit $ Map.keys rtbl
                        msg = MsgSigned { _msg_src = nid
                                        , _msg_dst = rid
                                        , _msg_epoch = epoch
                                        , _msg_payload = MkRouteMsg rmsg
                                        , _msg_sign = emptySign
                                        }
                        raw = toStrict $ encode msg
                    -- direct send to neighbor
                    in do liftIO $ _conn_tx_cb conn $ sign raw
               Nothing -> return ()
    _ -> return ()

  performEvent_ $ ffor rxMsgE $ \(rid, msg) ->
    case msg of
    -- msg ogm
    MsgEnvelopedSignal { _msg_envelope = EvpOGM hop } -> do
      let ogm = msg
      let src = _msg_src ogm
      rtbl <- sample rtblB


      liftIO $ printf "  OGM from %s ..., src %s ...\n"
                      (take 7 $ show rid)
                      (take 7 $ show (_msg_src ogm))

      -- update route event
      liftIO $ rtblT (rid, _msg_src ogm, _msg_epoch ogm)
      let newRecord = case Map.lookup src rtbl of
                      Nothing -> True
                      Just re -> _msg_epoch ogm > _re_ts re

      when ((hop < 4) && newRecord) $ routeOGM rid ogm

    -- msg route
    MsgSigned { _msg_payload = MkRouteMsg rmsg } -> do
      let src = _msg_src msg
      case rmsg of
        MkRouteInit nidL -> do
          st <- Map.map snd <$> (sample $ current stD)
          -- only src is neighbor, and is client mode
          when (Map.lookup src st == Just (ConnReady ConnIsClient)) $ do
            --- just use current time as epoch
            epoch <- liftIO $ getEpochMs
            liftIO $ mapM_ (\x -> rtblT (src, x, epoch)) nidL
        _ -> return ()

      return ()

  return (rtblB, route)
