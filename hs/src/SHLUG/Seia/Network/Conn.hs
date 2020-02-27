{-# language FlexibleContexts #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}

module SHLUG.Seia.Network.Conn ( connNew
                               , ConnState(..)
                               , Conn(..)
                               , ConnConf(..)
                               , ConnType(..)
                               , connStEnd
                               ) where

import SHLUG.Seia.Type
import SHLUG.Seia.Msg
import SHLUG.Seia.Msg.Payload

import SHLUG.Seia.Rt
import SHLUG.Seia.Helper
import SHLUG.Seia.Conf

import Data.ByteString ( ByteString(..) )
import Data.Text (Text(..))
import Data.Word (Word64)
import Data.Time

import Data.Binary
import Data.Int

import GHC.Generics (Generic)

import Data.ByteString.Lazy (fromStrict, toStrict)

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Exception (SomeException)
import Control.Lens ((^.))
import Control.Monad (when, unless, forM_)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))

import Control.Concurrent (forkIO, threadDelay, myThreadId, killThread)

import Data.IORef
import qualified Data.Text as T
import Data.Maybe (isNothing, isJust, fromJust)

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
                                   , jsg, js1
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
                           , _conn_msg_sign :: ByteString -> ByteString
                           , _conn_nid_exist :: Bool -- for MkRTCRes
                           }

data Conn = MkConn { _conn_tx_cb :: ByteString -> JSM ()
                   -- signed message, send to _conn_nid

                   -- verifyed rtc message to this node
                   -- and src is _conn_dst
                   , _conn_rtc_rx_cb :: (Word64, RTCMsg) -> JSM ()
                   }

promiseH0 :: (MonadCatch m, DOM.MonadJSM m) => DOM.PromiseRejected -> m (Maybe a)
promiseH0 e = do
  DOM.liftJSM $ consoleLog ("promise rejected", DOM.rejectionReason e)
  return Nothing

-- this can not catch JSException, why ?
jsexceptionH0 :: (MonadCatch m, DOM.MonadJSM m) => JSException -> m (Maybe a)
jsexceptionH0 e = do
  DOM.liftJSM $ consoleLog ("js exception", show e)
  return Nothing

exceptionH0' :: (MonadCatch m, DOM.MonadJSM m) => Bool -> SomeException -> m (Maybe e)
exceptionH0' verbose e = do
  when (verbose) $ DOM.liftJSM $ consoleLog ("js exception", show e)
  return Nothing

exceptionH0 = exceptionH0' False

peerConnCfg :: [Text] -> JSM JSVal
peerConnCfg ts = do
    urls <- obj
    (urls <# "urls") ts

    res <- obj
    (res <# "iceServers") [urls]

    toJSVal res

createConnection :: ConnConf ->
                    IORef ConnState ->
                    IORef (Maybe DOM.RTCPeerConnection) ->
                    IORef (Maybe DOM.RTCDataChannel) ->
                    IORef Int64 ->
                    Maybe JSVal -> JSM ()
createConnection c stRef pcRef dcRef tsRef remoteSdp = do
  pc <- peerConnCfg (_conn_turn_server c) >>=
        new (jsg "RTCPeerConnection") >>=
        (return . DOM.RTCPeerConnection)
  let isOffer = isNothing remoteSdp
  liftIO $ atomicWriteIORef pcRef (Just pc)

  DOM.on pc DOM.iceCandidate $ do
    e <- DOM.event
    can <- DOME.getCandidate e
    liftJSM $ do
      can' <- strToText <$> (jsg "JSON"  ^. js1 "stringify" can >>= valToStr)

      let msg = MkRTCSignal RTCCandidate can'

      done <- valIsNull can

      when (not done) $ sendRTCMsg c msg

      --  consoleLog x
      --if done then consoleLog "done" else consoleLog can
      return ()

    return ()

  DOM.on pc (unsafeEventName (toJSString "datachannel")) $ do
     e <- DOM.event
     dc <- DOME.getChannel e
     liftJSM $ do
       -- NOTE: should setup dc(callback) as soon as possible
       -- should not insert any IO op between
       setupDataChannel c stRef pcRef dcRef tsRef dc
       liftIO $ printf "  %s: on data channel\n" (show $ _conn_remote c)
       checkDC c stRef dc

  dc' <- if isOffer
         then Just <$> DOM.createDataChannel pc "ch0" Nothing
         else return Nothing

  -- NOTE: should set remote sdp before answer
  forM_ remoteSdp $ \x -> DOM.setRemoteDescription pc $ DOM.RTCSessionDescriptionInit x

  --consoleLog $ connMap ^. js remotePeer

  -- TODO, create ans & off is promise
  sdp <- if isJust remoteSdp
         then DOM.createAnswer pc Nothing
         else DOM.createOffer pc Nothing

  --consoleLog sdp

  --- TODO, this is promise, maybe reject
  DOM.setLocalDescription pc sdp
  sdp' <- strToText <$> (jsg "JSON"  ^. js1 "stringify" sdp >>= valToStr)

  -- insert dc, should insert after last function
  -- which throw PromiseRejected exception
  mapM_ (setupDataChannel c stRef pcRef dcRef tsRef) dc'

  if isJust remoteSdp
  then sendRTCMsg c $ MkRTCSignal RTCAnswer sdp'
  else sendRTCMsg c $ MkRTCSignal RTCOffer sdp'

checkDC :: ConnConf -> IORef ConnState -> DOM.RTCDataChannel -> JSM ()
checkDC c stRef dc = do
  st <- RTCDataChannel.getReadyState dc
  when (st == Enums.RTCDataChannelStateOpen) $
    updateSt c stRef undefined $ ConnReady (_conn_type c)
  liftIO $ printf " dc st -> %s\n" (show st)
  return ()

onRTCRx :: ConnConf ->
           IORef ConnState ->
           IORef (Maybe DOM.RTCPeerConnection) ->
           IORef (Maybe DOM.RTCDataChannel) ->
           IORef Int64 ->
           (Word64, RTCMsg) -> JSM ()
onRTCRx c stRef pcRef dcRef tsRef (epoch, msg) = do
  liftIO $ printf "    on rtc rx: %s\n" (sss 50 msg) >> IO.hFlush IO.stdout

  pc' <- liftIO $ readIORef pcRef
  case msg of
    MkRTCReq ver ->
      if ver /= _conn_main_ver c
      then do updateSt c stRef pcRef ConnFail
              sendRTCMsg c (MkRTCRes RTCMsgResIncompatiable)
      else do updateSt c stRef pcRef ConnSignal
              sendRTCMsg c (MkRTCRes if _conn_nid_exist c
                                     then RTCMsgResExist else RTCMsgResOK)
    MkRTCRes res ->
      if res == RTCMsgResIncompatiable -- TODO, check ResExist
      then do updateSt c stRef pcRef ConnFail
              liftIO $ putStrLn $ "rtc req fail: " ++ show res
      else do updateSt c stRef pcRef ConnSignal
              createConnection c stRef pcRef dcRef tsRef Nothing
    MkRTCSignal tp str ->
      do --- NOTE, wrtc will throw exception for null, "", {}
         jv <- jsg "JSON" ^. js1 "parse" str
         when (str == T.pack "null") $ consoleLog jv

         case (tp, pc') of
           (RTCOffer,  _) -> createConnection c stRef pcRef dcRef tsRef (Just jv)
           (RTCAnswer, Just pc) ->
             catch (DOM.setRemoteDescription pc (DOM.RTCSessionDescriptionInit jv))
                   (fmap (const ()) . promiseH0)
           (RTCCandidate, Just pc) ->
             catch (DOM.addIceCandidate pc (DOM.RTCIceCandidate jv))
                   (fmap (const ()) . promiseH0)
           _ -> do liftIO $ putStrLn $ "pc is not exist for " ++ show tp
                   updateSt c stRef pcRef ConnFail

  return ()


procMsg :: ConnConf -> UTCTime -> IORef Int64 -> ByteString -> JSM ()
procMsg c t0 tsRef payload = do
  -- update timestamp
  liftIO $ getEpochMs >>= atomicWriteIORef tsRef
  case decodeOrFail (fromStrict payload) of
       -- fail to decode
       Left _ -> return ()
       -- heart beat
       Right (_, _, MsgHeartbeat) -> do
             t <- liftIO getCurrentTime
             let dt = diffUTCTime t t0
             when (floor (dt*2) `mod` 120 == 0) $
                  liftIO $ printf "----> uptime %s : %s\n"
                                  (sss 8 (_conn_remote c)) (show dt)
             --liftIO $ printf "----> hb: %s, up %s\n"
             --                (show t) (show dt)

       Right (_, _, msg) -> do
             when (msgVerify payload) do
                  liftJSM $ _conn_rx_cb c $ (msg, payload)

-- TODO, check first message with 2 x N tolerantion, then 1 x N
heartbeatChecker :: JSVal -> ConnConf ->
                    IORef ConnState ->
                    IORef (Maybe DOM.RTCPeerConnection) ->
                    IORef Int64 -> DOM.RTCDataChannel -> JSM ()
heartbeatChecker pkt c stRef pcRef tsRef dc = do
  -- send heart beat,
  -- NOTE, current data channel might not in 'open' state,
  -- because remote might recv timeout and close channel
  catch (RTCDataChannel.sendView dc (DOM.ArrayBufferView pkt))
        (fmap (const ()) . exceptionH0)
  -- delay 500
  liftIO $ threadDelay $ 500 * 1000
  -- check
  now <- liftIO getEpochMs
  ts <- liftIO $ readIORef tsRef

  if now - ts > floor (_cc_conn_heartbeat_timeout confConst * 1000)
  then updateSt c stRef pcRef ConnTimeout
  else heartbeatChecker pkt c stRef pcRef tsRef dc

setupDataChannel :: ConnConf ->
                    IORef ConnState ->
                    IORef (Maybe DOM.RTCPeerConnection) ->
                    IORef (Maybe DOM.RTCDataChannel) ->
                    IORef Int64 ->
                    DOM.RTCDataChannel -> JSM ()
setupDataChannel c stRef pcRef dcRef tsRef dc = do
  let nid = _conn_remote c
  t0 <- liftIO getCurrentTime

  DOM.on dc RTCDataChannel.open $ do
    liftJSM $ updateSt c stRef pcRef (ConnReady (_conn_type c))
    t <- liftIO $ getEpochMs -- Int
    liftIO $ atomicWriteIORef tsRef t
    liftIO $ printf "  %s: data channel open\n" (show nid)
    ctx <- askJSM

    let bs = toStrict $ encode $ MsgHeartbeat
    pkt <- liftJSM $ bs_to_u8a bs >>= u8a_to_jsval

    liftIO $ forkIO $ runJSM (heartbeatChecker pkt c stRef pcRef tsRef dc) ctx
    return ()

  DOM.on dc RTCDataChannel.message $ do
    ev <- DOM.event
    dat <- DOM.getData ev

    bs' <- liftJSM $ jsval_to_bs dat
    case bs' of
      Just bs -> liftJSM $ procMsg c t0 tsRef bs
      Nothing -> do
        liftIO $ printf "%s: recv msg is not u8a/ab, drop\n" (show nid)
        liftJSM $ consoleLog dat

  DOM.on dc RTCDataChannel.error $ do
    liftIO $ printf "%s: data channel err\n" (show nid)

  DOM.on dc RTCDataChannel.closeEvent $ do
    liftJSM $ updateSt c stRef pcRef ConnFail
    liftIO $ printf "%s: data channel closed\n" (show nid)

  liftIO $ atomicWriteIORef dcRef (Just dc)

  return ()

onTx :: ConnConf -> IORef (Maybe DOM.RTCDataChannel) ->
        ByteString -> JSM ()
onTx c dcRef payload = do
  u8a <- bs_to_u8a payload
  dc' <- readIORef dcRef

  res <- runMaybeT $ do
    when (isNothing dc') $ fail "dc is not exist"
    let dc = fromJust dc'

    -- not check dc state, just catch any exception
    v <- liftJSM $ u8a_to_jsval u8a
    liftJSM $ catch (RTCDataChannel.sendView dc (DOM.ArrayBufferView v))
                    (fmap (const ()) . exceptionH0)

  when (res == Nothing) $
    liftIO $ printf "%s: tx msg drop, dc is not exist or not open\n"
               (show $ _conn_remote c)

  return ()

sendRTCMsg :: ConnConf -> RTCMsg -> JSM ()
sendRTCMsg c rmsg = do
  let msg = MsgSigned { _msg_src = _conn_local c
                      , _msg_dst = _conn_remote c
                      , _msg_epoch = 0
                      , _msg_payload = MkRTCMsg rmsg
                      , _msg_sign = emptySign
                      }
  msg1 <- msgFillEpoch msg
  let dat = (_conn_msg_sign c) (toStrict $ encode msg1)
  let sig = msgGetSignature dat
  let msg2 = msg1 { _msg_sign = sig }
  (_conn_rx_cb c) (msg2, dat)

updateSt :: ConnConf -> IORef ConnState ->
            IORef (Maybe DOM.RTCPeerConnection) -> ConnState -> JSM ()
updateSt c stRef pcRef st = do
  -- avoid race condition
  old <- liftIO $ atomicModifyIORef' stRef
                                     (\x -> if stEnd x then (x, x) else (st, x))
  unless (stEnd old) $ do
         when (stEnd st) $ do
              pc' <- liftIO $ atomicModifyIORef' pcRef (\x -> (Nothing, x))
              mapM_ RTCPeerConnection.close pc'
         _conn_st_cb c $ st

connNew :: (MonadJSM m) => ConnConf -> m Conn
connNew c = do
  pcRef <- liftIO $ newIORef Nothing
  dcRef <- liftIO $ newIORef Nothing

  -- last recv time stamp
  tsRef <- liftIO $ newIORef (-1)
  stRef <- liftIO $ newIORef ConnIdle

  liftIO $ printf "conn new: %s -> %s\n"
                  (show $ _conn_local c)
                  (show $ _conn_remote c)

  -- NOTE, before return, all updateSt to outside will drop
  liftJSM $ updateSt c stRef pcRef ConnIdle

  -- init rtc request to remote node
  when (_conn_type c == ConnIsClient) $
    liftJSM $ sendRTCMsg c $ MkRTCReq (_conn_main_ver c)

  -- TODO, should recv RTCRes in 5sec
  -- NOTE: only 1 timeout/fail should output
  -- or there will be race condition.
  ctx <- liftJSM askJSM
  liftIO $ forkIO $ do
    let timeout = do runJSM (updateSt c stRef pcRef ConnTimeout) ctx
                     myThreadId >>= killThread

    -- should become ConnSignal within 10sec
    threadDelay $ (_cc_conn_req_timeout confConst) * 1000 * 1000
    st1 <- readIORef stRef
    when (st1 == ConnIdle) timeout

    -- then, should finish Signal within 30sec
    threadDelay $ (_cc_conn_signal_timeout confConst) * 1000 * 1000
    st2 <- readIORef stRef
    when (st2 == ConnSignal) timeout

  return MkConn { _conn_tx_cb = onTx c dcRef
                , _conn_rtc_rx_cb = onRTCRx c stRef pcRef dcRef tsRef
                }
