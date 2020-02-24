{-# language TupleSections #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}

module SHLUG.Seia.Network.Route ( RouteEntry(..)
                                , routeSetup
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
                , NID -> ByteString -> Performable m ())
routeSetup nid sign mqtt_txT mqtt_stateD stE stD rxOgmE = do

  (rtblE, rtblT) <- newTriggerEvent
  rtblB <- accumB rtblUp Map.empty rtblE

  -- Conn life-cycle monitor for rtbl
  performEvent_ $ ffor stE $ \(nid, x) -> case x of
    Right st | connStEnd st -> liftIO $ rtblT (nid, nid, 0)
    Right (ConnReady _) -> liftIO $ getEpochMs >>= (rtblT . (nid, nid, ))
    _ -> return ()

  let route dst raw = do
        liftIO $ printf "  route msg to %s\n" (show dst)

        rtbl <- sample rtblB
        mqttSt <- sample $ current mqtt_stateD

        cmap <- Map.map fst <$> sample (current stD)
        -- now, rtbl will only containing routable node(include neighbor),
        -- rtblNextConn will not check cmap before check rtbl
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
        let ogm = MsgOGM { _msg_type = '\2'
                         , _msg_src = nid
                         , _msg_epoch = 0
                         , _msg_sign = emptySign
                         , _msg_hop = 0
                         }
        ogm1 <- liftIO $ msgFillEpoch ogm
        let raw = sign $ toStrict $ encode ogm1
        return $ decode $ fromStrict raw

  let sendOGM = genOGM >>= routeOGM nid
  -- send OGM every 20 sec
  tick20 <- liftIO getCurrentTime >>= tickLossy 20
  performEvent_ $ ffor tick20 $ const sendOGM

  -- when send OGM to first online neighbor
  performEvent_ $ ffor stE $ \(nid, x) -> case x of
    Right (ConnReady _) -> do
      st <- sample $ current stD
      when (L.null $ L.delete nid (Map.keys st)) sendOGM
    _ -> return ()


  performEvent_ $ ffor rxOgmE $ \(rid, ogm) -> do
      let hop = _msg_hop ogm
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

      return ()

  return (rtblB, route)
