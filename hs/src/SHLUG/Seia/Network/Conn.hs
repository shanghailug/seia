{-# language FlexibleContexts #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language TupleSections #-}

module SHLUG.Seia.Network.Conn ( connNew
                               , connInit
                               , ConnState(..)
                               , Conn(..)
                               , ConnConf(..)
                               , ConnType(..)
                               , connStEnd
                               , connDummy
                               ) where

import SHLUG.Seia.Type
import SHLUG.Seia.Msg
import SHLUG.Seia.Msg.Payload

import SHLUG.Seia.Rt
import SHLUG.Seia.Helper
import SHLUG.Seia.Conf
import SHLUG.Seia.Log

import Data.ByteString ( ByteString(..) )
import Data.Text (Text(..))
import Data.Word (Word64)
import Data.Time

import Data.Binary
import Data.Int

import GHC.Generics (Generic)

import Data.ByteString.Lazy (fromStrict, toStrict)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Exception (SomeException)
import Control.Lens ((^.))
import Control.Monad (when, unless, forM_, forever)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))

import Control.Concurrent (forkIO, threadDelay, myThreadId, killThread, ThreadId)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import Data.IORef
import qualified Data.Text as T
import Data.Maybe (isNothing, isJust, fromJust)

import GHC.Stack ( HasCallStack )

import Control.Concurrent.STM (atomically)
import System.IO.Unsafe (unsafePerformIO)

import qualified System.IO as IO

import Text.Printf
import Reflex

import Language.Javascript.JSaddle ( JSM(..), MonadJSM(..)
                                   , liftJSM
                                   , askJSM, runJSM
                                   , JSContextRef
                                   , JSException(..)
                                   , JSVal(..), toJSVal
                                   , toJSString
                                   , strToText, valToStr, valIsNull
                                   , js1, js
                                   , obj, new
                                   , (<#)
                                   )
import JavaScript.Cast (cast)

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.RTCPeerConnection as DOM
import qualified GHCJS.DOM.RTCIceCandidateEvent as DOME
import qualified GHCJS.DOM.RTCDataChannelEvent as DOME
import qualified GHCJS.DOM.RTCIceCandidate as DOM
import qualified GHCJS.DOM.RTCDataChannel as RTCDataChannel

-- for close
import qualified GHCJS.DOM.RTCPeerConnection as RTCPeerConnection

import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.MessageEvent as DOM
import qualified GHCJS.DOM.Enums as Enums

import GHCJS.DOM.EventTargetClosures (unsafeEventName)



data ConnType = ConnIsClient | ConnIsServer deriving (Eq, Show)

data ConnState = ConnIdle |
                 ConnSignal |
                 ConnReady ConnType |
                 ConnTimeout |
                 ConnFail
               deriving (Eq, Show)

stEnd :: ConnState -> Bool
stEnd ConnTimeout = True
stEnd ConnFail = True
stEnd _ = False

connStEnd = stEnd

{-
Conn

pure IO(callback) driven, to live inside performEvent_

when receive message from remote node
1. when receivce MsgHB, update watch dog
2. when receivce other message, verify, if pass, relay to _conn_rx

when event on _conn_tx
1. send to remote node

when event on _conn_rtc_rx
1. decode payload to RTCMsg, note, need handle exception
2. update state of RTCPeerConnection & RTCDataChannel or other
3. update Conn state
-}



{-
All event:

ext:
1. new connection
2. tx message
3. rtc message

int:
1. iceCandidate(RTCPeerConnection)
2. datachannel(RTCPeerConnection)
   : should do some direct process before send to TChan
3. connectionstatechange(RTCPeerConnection)
4. open(RTCDataChannel)
5. message(RTCDataChannel)
6. error(RTCDataChannel)
7. closeEvent(RTCDataChannel)
8. timeout(signal/connect)
9. heartbeat

-}

data ConnConf = MkConnConf { _conn_local :: NID
                           , _conn_remote :: NID
                           , _conn_main_ver :: Int
                           -- this conn is server mode, recv connection,
                           -- need check heartbeat
                           -- or is client mode, create connection,
                           -- need send heartbeat
                           , _conn_type :: ConnType
                           , _conn_st_cb :: ConnState -> JSM ()
                           -- verified message, directly from dst
                           -- or from this node, for RTC
                           , _conn_rx_cb :: (Msg, ByteString) -> JSM ()
                           , _conn_turn_server :: [Text]
                           , _conn_rx_cb' :: JSM () -- for status up
                           , _conn_rtt_cb :: Int -> JSM ()
                           }

data Conn = MkConn { _conn_tx_cb :: ByteString -> JSM ()
                   -- signed message, send to _conn_nid

                   -- verifyed rtc message to this node
                   -- and src is _conn_dst
                   , _conn_rtc_rx_cb :: (Word64, RTCMsg) -> JSM ()
                   }

type LogJSM = LogIOM JSM

connDummy :: Conn
connDummy = MkConn (const (return ())) (const (return ()))

promiseH0 :: (MonadCatch m, MonadJSM m) => DOM.PromiseRejected -> m (Maybe a)
promiseH0 e = do
  DOM.liftJSM $ consoleLog ("promise rejected", DOM.rejectionReason e)
  return Nothing

-- this can not catch JSException, why ?
jsexceptionH0 :: (MonadCatch m, MonadJSM m) => JSException -> m (Maybe a)
jsexceptionH0 e = do
  DOM.liftJSM $ consoleLog ("js exception", show e)
  return Nothing

exceptionH0' :: (MonadCatch m, DOM.MonadJSM m) => Bool -> SomeException -> m (Maybe e)
exceptionH0' verbose e = do
  when (verbose) $ DOM.liftJSM $ consoleLog ("js exception", show e)
  return Nothing

--exceptionH0 = exceptionH0' False

peerConnCfg :: [Text] -> JSM JSVal
peerConnCfg ts = do
    urls <- obj
    (urls <# "urls") ts

    res <- obj
    (res <# "iceServers") [urls]

    toJSVal res

createConnection :: HasCallStack =>
                    ConnEntry -> ConnResource -> Maybe JSVal -> JSM ConnEntry
createConnection ent res remoteSdp = do
  let logJSM = _cr_log res
  let c = _ce_conf ent
  let nid = _conn_remote c
  logJSM D $ T.pack $ printf "--- t1"

  pc <- peerConnCfg (_conn_turn_server c) >>=
        new (js_rt ^. js "RTCPeerConnection") >>=
        (return . DOM.RTCPeerConnection)
  logJSM I $ T.pack $ printf ">>> pc create: %s" (sss 8 nid)
  let isOffer = isNothing remoteSdp

  logJSM D $ T.pack $ printf "--- t2"

  DOM.on pc DOM.iceCandidate $ do
    e <- DOM.event
    can <- DOME.getCandidate e
    liftIO $ sendCmdIO nid (CmdPcIce can)

  DOM.on pc (unsafeEventName (toJSString "datachannel")) $ do
     e <- DOM.event
     dc <- DOME.getChannel e
     liftJSM $ do
             -- NOTE: should setup dc(callback) as soon as possible
             -- should not insert any IO op between
             setupDataChannel ent res dc
             sendCmdJSM nid (CmdPcDc dc)
             logJSM D $ T.pack $ printf "%s: on data channel" (sss 8 nid)

  -- for debug, add more state event handler
  DOM.on pc DOM.connectionstatechange $ do
    e <- DOM.event
    let jv = DOM.unEvent e --
    st <- liftJSM $ DOM.getConnectionState pc

    liftJSM $ logJSM D $ T.pack $
                         printf "%s: conn state %s"
                                (sss 8 $ _conn_remote c) (show st)

  logJSM D $ T.pack $ printf "--- t3"

  when isOffer $ do dc <- DOM.createDataChannel pc "ch0" Nothing
                    setupDataChannel ent res dc
                    sendCmdJSM nid (CmdPcDc dc)

  logJSM D $ T.pack $ printf "--- t4"
  -- NOTE: should set remote sdp before answer
  forM_ remoteSdp $ \x -> DOM.setRemoteDescription pc $ DOM.RTCSessionDescriptionInit x
  logJSM D $ T.pack $ printf "--- t5"
  --consoleLog $ connMap ^. js remotePeer

  -- TODO, create ans & off is promise, here will reject(actual happen)
  -- Need catch js exception
  sdp <- if isJust remoteSdp
         then DOM.createAnswer pc Nothing
         else DOM.createOffer pc Nothing
  logJSM D $ T.pack $ printf "--- t6"
  --consoleLog sdp

  --- TODO, this is promise, maybe reject
  DOM.setLocalDescription pc sdp
  sdp' <- strToText <$> (js_rt ^. js "JSON"  ^. js1 "stringify" sdp >>= valToStr)
  logJSM D $ T.pack $ printf "--- t7"

  -- insert dc, should insert after last function
  -- which throw PromiseRejected exception

  logJSM D $ T.pack $ printf "--- t8"
  if isJust remoteSdp
  then sendRTCMsg c res $ MkRTCSignal RTCAnswer sdp'
  else sendRTCMsg c res $ MkRTCSignal RTCOffer sdp'

  return $ ent { _ce_pc = Just pc }

onRTCRx :: HasCallStack =>
           ConnEntry -> ConnResource -> (Word64, RTCMsg) -> JSM (Maybe ConnEntry)
onRTCRx ent res (epoch, msg) = do
  let logJSM = _cr_log res
  let c = _ce_conf ent
  let st = _ce_st ent
  let tp = _conn_type c
  let pc' = _ce_pc ent
  let rstr = sss 8 $ _conn_remote c

  logJSM D $ T.pack $ printf "rtc:rx:%s: %s" rstr (sss 50 msg)

  case (msg, tp, st) of
    (MkRTCReq ver, ConnIsServer, ConnIdle) ->
      if ver /= _conn_main_ver c
      then do sendRTCMsg c res (MkRTCRes RTCMsgResIncompatiable)
              updateSt ent res ConnFail
              return $ Nothing
      else do sendRTCMsg c res (MkRTCRes RTCMsgResOK)
              let st' = ConnSignal
              updateSt ent res st'
              return $ Just $ ent { _ce_st = st' }

    -- in none idle state
    (MkRTCReq ver, ConnIsServer, _) ->
      do sendRTCMsg c res (MkRTCRes RTCMsgResExist)
         return $ Just ent

    (MkRTCRes res1, ConnIsClient, ConnIdle) ->
      if (res1 == RTCMsgResIncompatiable) ||
         (res1 == RTCMsgResExist)
      then do updateSt ent res ConnFail
              logJSM W $ T.pack $ "rtc req fail: " ++ show res1
              return Nothing
      else do let st' = ConnSignal
              let ent' = ent { _ce_st = st' }
              updateSt ent res st'
              ent'' <- createConnection ent' res Nothing
              return $ Just ent''

    (MkRTCSignal tp str, _, ConnSignal) ->
      do --- NOTE, wrtc will throw exception for null, "", {}
         jv <- js_rt ^. js "JSON" ^. js1 "parse" str
         when (str == T.pack "null") $ consoleLog jv

         case (tp, pc', _conn_type c) of
           (RTCOffer,  Nothing, ConnIsServer) -> do
             logJSM D $ T.pack $ printf "webrtc:off:%s -> %s" rstr str
             ent' <- createConnection ent res (Just jv)
             return $ Just ent'

           -- ignore redundant offer
           (RTCOffer,  Just _, ConnIsServer) -> return (Just ent)

           (RTCAnswer, Just pc, ConnIsClient) -> do
             logJSM D $ T.pack $ printf "webrtc:ans:%s -> %s" rstr str
             catch (DOM.setRemoteDescription pc (DOM.RTCSessionDescriptionInit jv))
                   (fmap (const ()) . promiseH0)
             return $ Just ent

           (RTCCandidate, Just pc, _) -> do
             logJSM D $ T.pack $ printf "webrtc:can:%s -> %s" rstr str
             catch (DOM.addIceCandidate pc (DOM.RTCIceCandidate jv))
                   (fmap (const ()) . promiseH0)
             return $ Just ent

           _ -> do logJSM W $ T.pack $ "pc is not exist or connection type incorrect"
                   updateSt ent res ConnFail
                   return Nothing

    _ -> do logJSM W $ T.pack "wrong type of message"
            return $ Just ent


sendJSVal :: HasCallStack => JSVal -> DOM.RTCDataChannel -> JSM ()
sendJSVal pkt dc = do
  catch (RTCDataChannel.sendView dc (DOM.ArrayBufferView pkt))
        (fmap (const ()) . exceptionH0' True)

heartbeatPkt :: HasCallStack => JSM JSVal
heartbeatPkt = do
  let bs = toStrict $ encode $ MsgHeartbeat
  u8a <- bs_to_u8a bs
  u8a_to_jsval u8a

setupDataChannel :: HasCallStack =>
                    ConnEntry ->
                    ConnResource ->
                    DOM.RTCDataChannel -> JSM ()
setupDataChannel ent res dc = do
  let c = _ce_conf ent
  let nid = _conn_remote c
  let logJSM = _cr_log res

  DOM.on dc RTCDataChannel.open $ liftIO $ sendCmdIO nid CmdDcOpen

  DOM.on dc RTCDataChannel.message $ do
    ev <- DOM.event
    dat <- DOM.getData ev

    bs' <- liftJSM $ jsval_to_bs dat
    case bs' of
      Just bs -> liftIO $ sendCmdIO nid (CmdDcMsg bs)
      Nothing -> do
        liftJSM $ logJSM W $ T.pack $
                  printf "%s: recv msg is not u8a/ab, drop" (sss 8 $ nid)
        liftJSM $ consoleLog dat

  DOM.on dc RTCDataChannel.error $ do
    liftIO $ sendCmdIO nid CmdDcErr
    liftJSM $ logJSM W $ T.pack $ printf "%s: data channel err" (show nid)

  DOM.on dc RTCDataChannel.closeEvent $ do
    liftIO $ sendCmdIO nid CmdDcClose
    liftJSM $ logJSM I $ T.pack $ printf "%s: data channel closed" (show nid)

  return ()


sendRTCMsg :: HasCallStack => ConnConf -> ConnResource -> RTCMsg -> JSM ()
sendRTCMsg c res rmsg = do
  let logJSM = _cr_log res
  logJSM D $ T.pack $ printf "rtc:tx:%s: %s" (sss 8 $ _conn_remote c)
                                             (sss 50 rmsg)

  let msg = MsgSigned { _msg_src = _conn_local c
                      , _msg_dst = _conn_remote c
                      , _msg_epoch = 0
                      , _msg_payload = MkRTCMsg rmsg
                      , _msg_sign = emptySign
                      }
  msg1 <- msgFillEpoch msg
  let dat = (_cr_msg_sign res) (toStrict $ encode msg1)
  let sig = msgGetSignature dat
  let msg2 = msg1 { _msg_sign = sig }

  (_conn_rx_cb c) (msg2, dat)

updateSt :: HasCallStack => ConnEntry -> ConnResource -> ConnState -> JSM ()
updateSt ent res st = do
  let logJSM = _cr_log res
  let c = _ce_conf ent
  let old = _ce_st ent
  let pc' = _ce_pc ent
  logJSM D $ T.pack $ printf "webrtc:st:%s -> %s => %s" (sss 8 $ _conn_remote c)
                             (show old) (show st)
  unless (stEnd old) $ do
         when (stEnd st) $ connEntryRelease ent res
  _conn_st_cb c $ st

data Cmd = CmdNew ConnConf |
           CmdTx ByteString |
           CmdRtcRx (Word64,RTCMsg) |
           CmdPcIce DOM.RTCIceCandidate |
           CmdPcDc DOM.RTCDataChannel |
           CmdPcState |
           CmdDcOpen |
           CmdDcMsg ByteString |
           CmdDcErr |
           CmdDcClose |
           CmdTimeoutS | -- signal
           CmdTimeoutC | -- connection
           CmdHbCheck

data ConnEntry = MkConnEntry { _ce_conf :: ConnConf
                             , _ce_pc :: Maybe DOM.RTCPeerConnection
                             , _ce_dc :: Maybe DOM.RTCDataChannel
                             , _ce_st :: ConnState
                             , _ce_t0 :: UTCTime -- setup time
                             , _ce_ts :: Int64 -- last message timestamp
                             , _ce_hb :: Int64 -- last heatbeat
                             , _ce_tid :: [ThreadId]
                             }

data ConnResource = MkConnResource { _cr_log :: LogJSM
                                   , _cr_msg_sign :: ByteString -> ByteString
                                   , _cr_ts :: TVar Int64 -- current ms, every 10ms
                                   }


connEntryRelease :: HasCallStack => ConnEntry -> ConnResource -> JSM ()
connEntryRelease ent res = do
  (_cr_log res) I $ T.pack $ printf ">>> release: %s"
             (sss 8 $ _conn_remote $ _ce_conf ent)
  mapM_ RTCPeerConnection.close (_ce_pc ent)
  mapM_ killThread (_ce_tid ent)
  return ()

{-# NOINLINE _cmdChan #-}
_cmdChan :: TChan (NID, Cmd)
_cmdChan = unsafePerformIO newTChanIO

sendCmdIO :: NID -> Cmd -> IO ()
sendCmdIO nid cmd = atomically $ writeTChan _cmdChan (nid, cmd)

sendCmdJSM :: NID -> Cmd -> JSM ()
sendCmdJSM nid cmd = liftIO $ sendCmdIO nid cmd

-- TODO: make outside directly observe 'Map NID ConnEntry'
connRun :: (HasCallStack) =>
           JSContextRef -> Map NID ConnEntry ->
           TChan (NID, Cmd) -> ConnResource -> JSM ()
connRun ctx m ch res = do
   let logJSM = _cr_log res
   (nid, cmd) <- atomically (readTChan _cmdChan)
   let ent' = Map.lookup nid m
   case (ent', cmd) of
        -- connection exist, skip
        (Just _, CmdNew c) -> do
              logJSM I $ T.pack $ printf "conn skip: %s -> %s"
                                         (sss 8 $ _conn_local c)
                                         (sss 8 $ _conn_remote c)
              connRun ctx m ch res
        -- connection new
        (Nothing, CmdNew c) -> do
              logJSM I $ T.pack $ printf "conn new: %s -> %s"
                                         (sss 8 $ _conn_local c)
                                         (sss 8 $ _conn_remote c)

              -- init rtc request to remote node
              when (_conn_type c == ConnIsClient) $
                   sendRTCMsg c res $ MkRTCReq (_conn_main_ver c)

              t0 <- liftIO getCurrentTime


              -- NOTE: here will have race condition,
              -- when tid1 send CmdTimeout* just before
              -- be killed in connEntryRelease
              tid1 <- liftIO $ forkIO $ do
              -- should become ConnSignal within _cc_conn_req_timeout
                 threadDelay $ (_cc_conn_req_timeout confConst) * 1000 * 1000
                 sendCmdIO nid CmdTimeoutS

                 -- then, should finish Signal within _cc_conn_signal_timeout
                 threadDelay $ (_cc_conn_signal_timeout confConst) * 1000 * 1000
                 sendCmdIO nid CmdTimeoutC

                 return ()

              let ent = MkConnEntry c Nothing Nothing ConnIdle t0 (-1) 0 [tid1]
              let m' = Map.insert nid ent m

              -- should updateSt, make outside known connection exist
              updateSt ent res ConnIdle

              connRun ctx m' ch res

        (Just ent, _) -> do
              e <- runJSM (connProc cmd nid ent res) ctx
              let m' = Map.update (const e) nid m
              connRun ctx m' ch res

        (Nothing, _) -> do
              logJSM I $ T.pack $ printf "skip cmd: %s" (sss 8 nid)
              connRun ctx m ch res

connInit :: (MonadJSM m, HasCallStack) =>
            LogJSM -> (ByteString -> ByteString) -> m ()
connInit logJSM msgSignFunc = do
  t0 <- liftIO getEpochMs
  ts <- liftIO $ newTVarIO t0
  liftIO $ forkIO $ forever do t <- liftIO getEpochMs
                               atomically $ writeTVar ts t
                               threadDelay (10 * 1000)

  let res = MkConnResource { _cr_log = logJSM
                           , _cr_ts = ts
                           , _cr_msg_sign = msgSignFunc
                           }
  ctx <- liftJSM askJSM
  liftIO $ forkIO $ connRun ctx Map.empty _cmdChan res
  return ()

connProc :: (HasCallStack) =>
            Cmd -> NID -> ConnEntry -> ConnResource -> JSM (Maybe ConnEntry)

-- tx
connProc (CmdTx payload) nid ent res = do
  let logJSM = _cr_log res
  let c = _ce_conf ent
  let dc' = _ce_dc ent

  u8a <- bs_to_u8a payload

  res <- runMaybeT $ do
    when (isNothing dc') $ fail "dc is not exist"
    let dc = fromJust dc'

    -- not check dc state, just catch any exception
    v <- liftJSM $ u8a_to_jsval u8a
    liftJSM $ sendJSVal v dc

  when (res == Nothing) $
    logJSM W $ T.pack $ printf "%s: tx msg drop, dc is not exist or not open"
                               (sss 8 nid)

  return $ Just ent

-- rtc rx
connProc (CmdRtcRx x) nid ent res = onRTCRx ent res x

-- pc candidate
connProc (CmdPcIce can) nid ent res = do
  let logJSM = _cr_log res
  let c = _ce_conf ent
  can' <- strToText <$> (js_rt ^. js "JSON"  ^. js1 "stringify" can >>=
                        valToStr)
  let msg = MkRTCSignal RTCCandidate can'
  done <- valIsNull can
  when (not done) $ sendRTCMsg c res msg

  return $ Just ent

-- pc datachannel
connProc (CmdPcDc dc) nid ent res = do
  let logJSM = _cr_log res
  t0 <- liftIO getCurrentTime
  return $ Just ent { _ce_dc = Just dc, _ce_t0 = t0 }

-- dc open
connProc CmdDcOpen nid ent res = do
  let logJSM = _cr_log res
  let c = _ce_conf ent
  let st' = ConnReady $ _conn_type c
  let dc' = _ce_dc ent
  updateSt ent res st'

  ts' <- liftIO $ getEpochMs -- Int
  logJSM I $ T.pack $ printf "%s: data channel open" (show nid)

  -- heart beat check, 500ms later
  liftIO $ forkIO $ threadDelay (500 * 1000) >> sendCmdJSM nid CmdHbCheck

  -- start send heartbeat from client
  pkt <- heartbeatPkt
  mapM_ (\dc -> when (_conn_type c == ConnIsClient) $ liftJSM $ sendJSVal pkt dc)
        dc'

  return $ Just ent { _ce_ts = ts', _ce_st = st' }

-- dc message
connProc (CmdDcMsg payload) nid ent res = do
  let logJSM = _cr_log res
  let c = _ce_conf ent
  let dc' = _ce_dc ent
  -- update timestamp
  ts' <- liftIO getEpochMs
  _conn_rx_cb' c -- update
  case decodeOrFail (fromStrict payload) of
       -- fail to decode
       Left _ -> return $ Just ent { _ce_ts = ts' }
       -- heart beat
       Right (_, _, MsgHeartbeat) -> do
             pkt <- heartbeatPkt
             liftIO $ forkIO $ threadDelay (500 * 1000) >>
                               mapM_ (sendJSVal pkt) dc'

             t <- liftIO getCurrentTime
             let t0 = _ce_t0 ent

             let dt = diffUTCTime t t0
             when (floor (dt*2) `mod` 120 == 0) $
                  logJSM D $ T.pack $ printf "----> uptime %s : %s\n"
                                             (sss 8 (_conn_remote c)) (show dt)

             ms <- liftIO getEpochMs
             let ms0 = _ce_hb ent

             when (ms0 > 0) $ do
                  let rtt = fromIntegral $ ms - ms0 - 1000
                  _conn_rtt_cb c $ rtt
                  logJSM D $ T.pack $ printf "----> %s: rtt %d ms"
                                             (sss 8 $ _conn_remote c)
                                             rtt
             return $ Just ent { _ce_ts = ts', _ce_hb = ms }

       Right (_, _, msg) -> do
             when (msgVerify payload) do
                  _conn_rx_cb c $ (msg, payload)
             return $ Just ent { _ce_ts = ts' }

-- dc error
connProc CmdDcErr nid ent res = return $ Just ent

-- dc close
connProc CmdDcClose nid ent res = do
  let st' = ConnFail
  updateSt ent res st'
  return Nothing

-- signal timeout, if we still in ConnIdle, then fail
connProc CmdTimeoutS nid ent res = do
  let logJSM = _cr_log res
  let st = _ce_st ent
  logJSM D $ T.pack $ printf "cmd:timeouts:%s: %s"
                             (sss 8 nid) (show st)
  if st == ConnIdle
  then updateSt ent res ConnTimeout >> return Nothing
  else return $ Just ent

-- connect timeout, if we still in ConnIdle or ConnSignal
connProc CmdTimeoutC nid ent res = do
  let st = _ce_st ent
  (_cr_log res) D $ T.pack $ printf "cmd:timeoutc:%s: %s"
                                    (sss 8 nid) (show st)

  if (st == ConnIdle) || (st == ConnSignal)
  then updateSt ent res ConnTimeout >> return Nothing
  else return $ Just ent

-- heart beat check
connProc CmdHbCheck nid ent res = do
  {-
  -- send heart beat,
  -- NOTE, current data channel might not in 'open' state,
  -- because remote might recv timeout and close channel
  catch (RTCDataChannel.sendView dc (DOM.ArrayBufferView pkt))
        (fmap (const ()) . exceptionH0' True)

  logJSM D $ T.pack $ printf "<---- %s" (sss 8 $ _conn_remote c)
  -}

  -- check
  now <- liftIO getEpochMs
  let ts = _ce_ts ent

  if now - ts > floor (_cc_conn_heartbeat_timeout confConst * 1000)
  then updateSt ent res ConnTimeout >> return Nothing
  else liftIO (forkIO $ threadDelay (500 * 1000) >> sendCmdIO nid CmdHbCheck) >>
       return (Just ent)

-- should not be called
connProc (CmdNew _) nid ent res = undefined

connNew :: (MonadJSM m, HasCallStack) => ConnConf -> m Conn
connNew c = do
  let ch = _cmdChan
  let rid = _conn_remote c

  liftIO $ sendCmdIO rid (CmdNew c)

  return MkConn { _conn_tx_cb = \x -> sendCmdIO rid (CmdTx x)
                , _conn_rtc_rx_cb = \x -> sendCmdIO rid (CmdRtcRx x)
                }
