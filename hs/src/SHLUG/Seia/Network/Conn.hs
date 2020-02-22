{-# language FlexibleContexts #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}

module SHLUG.Seia.Network.Conn ( connNew
                               , ConnState(..)
                               , Conn(..)
                               , ConnConf(..)
                               , ConnType(..)
                               ) where

import SHLUG.Seia.Type
import SHLUG.Seia.Msg

import SHLUG.Seia.Rt
import SHLUG.Seia.Helper

import Data.ByteString ( ByteString(..) )
import Data.Text (Text(..))
import Data.Word (Word64)

import Data.Binary
import Data.Int

import GHC.Generics (Generic)

import Data.ByteString.Lazy (fromStrict, toStrict)

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Exception (SomeException)
import Control.Lens ((^.))
import Control.Monad (when, forM_)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))

import Control.Concurrent (forkIO, threadDelay)

import Data.IORef
import qualified Data.Text as T
import Data.Maybe (isNothing, isJust, fromJust)

import qualified System.IO as IO

import Text.Printf
import Reflex

import Language.Javascript.JSaddle ( JSM(..), MonadJSM(..)
                                   , liftJSM
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

-- TODO: serialize of RTCMsg might incompatible during version change
data RTCMsgResType = RTCMsgResExist |
                     RTCMsgResIncompatiable |
                     RTCMsgResOK
                   deriving (Eq, Show, Generic)

instance Binary RTCMsgResType

data RTCSignalType = RTCOffer | RTCAnswer | RTCCandidate
                     deriving (Eq, Show, Generic)
instance Binary RTCSignalType

data RTCMsg =  MkRTCReq Int |
               MkRTCRes RTCMsgResType |
               MkRTCSignal RTCSignalType Text
            deriving (Eq, Show, Generic)

instance Binary RTCMsg

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
                           , _conn_rx_cb :: ByteString -> JSM ()
                           , _conn_turn_server :: [Text]
                           , _conn_msg_sign :: ByteString -> ByteString
                           , _conn_nid_exist :: Bool -- for MkRTCRes
                           }

data Conn m = MkConn { _conn_tx_cb :: ByteString -> m ()
                     -- signed message, send to _conn_nid

                     -- verifyed rtc message to this node
                     -- and src is _conn_dst
                     , _conn_rtc_rx_cb :: (Word64, ByteString) -> m ()
                     }

promiseH0 :: (MonadCatch m, DOM.MonadJSM m) => DOM.PromiseRejected -> m (Maybe a)
promiseH0 e = do
  DOM.liftJSM $ consoleLog ("promise rejected", DOM.rejectionReason e)
  return Nothing

jsexceptionH0 :: (MonadCatch m, DOM.MonadJSM m) => JSException -> m (Maybe a)
jsexceptionH0 e = do
  DOM.liftJSM $ consoleLog ("js exception", show e)
  return Nothing

exceptionH0 :: (MonadCatch m, DOM.MonadJSM m) => SomeException -> m (Maybe e)
exceptionH0 e = do
  DOM.liftJSM $ consoleLog ("js exception", show e)
  return Nothing

peerConnCfg :: [Text] -> JSM JSVal
peerConnCfg ts = do
    urls <- obj
    (urls <# "urls") ts

    res <- obj
    (res <# "iceServers") [urls]

    toJSVal res

createConnection :: ConnConf ->
                    IORef (Maybe DOM.RTCPeerConnection) ->
                    IORef (Maybe DOM.RTCDataChannel) ->
                    IORef Int64 ->
                    Maybe JSVal -> JSM ()
createConnection c pcRef dcRef tsRef remoteSdp = do
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
       setupDataChannel c dcRef tsRef dc
       liftIO $ printf "  %s: on data channel\n" (show $ _conn_local c)
       checkDC c dc

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
  mapM_ (setupDataChannel c dcRef tsRef) dc'

  if isJust remoteSdp
  then sendRTCMsg c $ MkRTCSignal RTCAnswer sdp'
  else sendRTCMsg c $ MkRTCSignal RTCOffer sdp'

checkDC :: ConnConf -> DOM.RTCDataChannel -> JSM ()
checkDC c dc = do
  st <- RTCDataChannel.getReadyState dc
  when (st == Enums.RTCDataChannelStateOpen) $
       _conn_st_cb c $ ConnReady (_conn_type c)
  liftIO $ printf " dc st -> %s\n" (show st)
  return ()

onRTCRx :: ConnConf ->
           IORef (Maybe DOM.RTCPeerConnection) ->
           IORef (Maybe DOM.RTCDataChannel) ->
           IORef Int64 ->
           (Word64, ByteString) -> JSM ()
onRTCRx c pcRef dcRef tsRef (epoch, raw) = do
  let msg = decode $ fromStrict raw :: RTCMsg
  liftIO $ printf "    on rtc rx: %s\n" (sss 50 msg) >> IO.hFlush IO.stdout

  pc' <- liftIO $ readIORef pcRef
  case msg of
    MkRTCReq ver ->
      if _conn_nid_exist c
      then do _conn_st_cb c $ ConnFail
              sendRTCMsg c (MkRTCRes RTCMsgResExist)
      else if ver /= _conn_main_ver c
           then do _conn_st_cb c $ ConnFail
                   sendRTCMsg c (MkRTCRes RTCMsgResIncompatiable)
           else do  _conn_st_cb c $ ConnSignal
                    sendRTCMsg c (MkRTCRes RTCMsgResOK)
    MkRTCRes res ->
      if res /= RTCMsgResOK
      then do _conn_st_cb c $ ConnFail
              liftIO $ putStrLn $ "rtc req fail: " ++ show res
      else do _conn_st_cb c $ ConnSignal
              createConnection c pcRef dcRef tsRef Nothing
    MkRTCSignal tp str ->
      do --- NOTE, wrtc will throw exception for null, "", {}
         jv <- jsg "JSON" ^. js1 "parse" str
         when (str == T.pack "null") $ consoleLog jv

         case (tp, pc') of
           (RTCOffer,  _) -> createConnection c pcRef dcRef tsRef (Just jv)
           (RTCAnswer, Just pc) ->
             catch (DOM.setRemoteDescription pc (DOM.RTCSessionDescriptionInit jv))
                   (fmap (const ()) . promiseH0)
           (RTCCandidate, Just pc) ->
             catch (DOM.addIceCandidate pc (DOM.RTCIceCandidate jv))
                   (fmap (const ()) . promiseH0)
           _ -> do liftIO $ putStrLn $ "pc is not exist for " ++ show tp
                   _conn_st_cb c $ ConnFail

  return ()

setupDataChannel :: ConnConf -> IORef (Maybe DOM.RTCDataChannel) ->
                    IORef Int64 ->
                    DOM.RTCDataChannel -> JSM ()
setupDataChannel c dcRef tsRef dc = do
  let nid = _conn_local c

  DOM.on dc RTCDataChannel.open $ do
    liftJSM $ (_conn_st_cb c) (ConnReady (_conn_type c))
    t <- liftIO $ getEpochMs -- Int
    liftIO $ atomicWriteIORef tsRef t
    liftIO $ printf "  %s: data channel open\n" (show nid)

  DOM.on dc RTCDataChannel.message $ do
    ev <- DOM.event
    dat <- DOM.getData ev

    u8a' <- liftJSM $ jsval_to_u8a dat
    case u8a' of
      Just u8a -> do bs <- liftIO $ u8a_to_bs u8a
                     -- todo, verify
                     liftJSM $ _conn_rx_cb c $ bs
      Nothing -> liftIO $ printf "%s: recv msg is not u8a, drop\n" (show nid)

  DOM.on dc RTCDataChannel.error $ do
    liftIO $ printf "%s: data channel err\n" (show nid)

  DOM.on dc RTCDataChannel.closeEvent $ do
    liftJSM $ _conn_st_cb c $ ConnFail
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
    st <- DOM.liftJSM $ RTCDataChannel.getReadyState dc
    when (st /= Enums.RTCDataChannelStateOpen) $
      fail "dc is not open"

    v <- liftJSM $ u8a_to_jsval u8a
    liftJSM $ RTCDataChannel.sendView dc (DOM.ArrayBufferView v)

  when (res == Nothing) $
    liftIO $ printf "%s: tx msg drop, dc is not exist or not open\n"
               (show $ _conn_local c)

  return ()

sendRTCMsg :: ConnConf -> RTCMsg -> JSM ()
sendRTCMsg c rmsg = do
  let msg = MsgSigned { _msg_type = '\4' -- TODO, should not hard encode
                      , _msg_src = _conn_local c
                      , _msg_dst = _conn_remote c
                      , _msg_epoch = 0 -- TODO
                      , _msg_payload = toStrict $ encode rmsg
                      , _msg_sign = emptySign
                      }
  msg1 <- msgFillEpoch msg
  let dat = (_conn_msg_sign c) (toStrict $ encode msg1)
  (_conn_rx_cb c) dat

connNew :: (MonadJSM m) => ConnConf -> m (Conn m)
connNew c = do
  pcRef <- liftIO $ newIORef Nothing
  dcRef <- liftIO $ newIORef Nothing

  -- last recv time stamp
  tsRef <- liftIO $ newIORef (-1)

  liftIO $ printf "conn new: %s -> %s\n"
                  (show $ _conn_local c)
                  (show $ _conn_remote c)

  liftJSM $ (_conn_st_cb c) ConnIdle
  -- init rtc request to remote node
  when (_conn_type c == ConnIsClient) $
    liftJSM $ sendRTCMsg c $ MkRTCReq (_conn_main_ver c)

  liftIO $ forkIO $ do
    threadDelay $ 10 * 1000 * 1000
    ts <- readIORef tsRef
    when (ts < 0) (_conn_st_cb c $ ConnTimeout)

  return MkConn { _conn_tx_cb = liftJSM . onTx c dcRef
                , _conn_rtc_rx_cb = liftJSM . onRTCRx c pcRef dcRef tsRef
                }
