{-# language DeriveGeneric, DeriveDataTypeable #-}
{-# language DuplicateRecordFields #-}
{-# language ForeignFunctionInterface, JavaScriptFFI #-}

module SHLUG.Seia.Msg
  ( Msg(..)
  , msgVerify
  , msgSign
  , msgGetSignature
  , emptySign
  , msgTrivalTest
  , msgFillEpoch
  ) where

import SHLUG.Seia.Type
import SHLUG.Seia.Helper
import SHLUG.Seia.Rt(sign, verify)

import SHLUG.Seia.Msg.Envelope
import SHLUG.Seia.Msg.Payload

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy(fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as UTF8

import Data.Binary
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import Data.Typeable
import Data.Data
import GHC.Generics
import Data.Int

import qualified Data.Text as T
import Data.Text (Text(..))

import qualified Data.List as L
import Data.Maybe (fromMaybe)

import JavaScript.TypedArray ( TypedArray(..)
                             , Uint8Array
                             , subarray
                             )
import Language.Javascript.JSaddle ( JSM(..), ghcjsPure )
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe(unsafePerformIO)

--import Test.QuickCheck
tpHeartbeart :: Word8
tpHeartbeart = 1

tpEnvelopedSignal :: Word8
tpEnvelopedSignal = 2

tpSigned :: Word8
tpSigned = 3

data Msg = MsgHeartbeat | -- '\1'
           -- type 2, MsgSign1, '\2'
           MsgEnvelopedSignal { _msg_src   :: !NID
                              , _msg_epoch :: !Word64
                              , _msg_sign  :: !ByteString -- 64 byte
                              -- envelope, can be modified without resign
                              , _msg_envelope :: Envelope
                            } |
           -- type 3
           MsgSigned { _msg_src     :: !NID
                     , _msg_dst     :: !NID
                     , _msg_epoch   :: !Word64
                     , _msg_payload :: Payload
                     , _msg_sign    :: !ByteString
                     }
           deriving (Eq, Show, Typeable, Data)
msgVerify :: ByteString -> Bool
msgVerify x = let
  tp = BS.head x
  len = BS.length x

  res' = fromMaybe (Right False) $
         L.lookup tp [ (tpHeartbeart     , Right True)
                     , (tpEnvelopedSignal, Left $ f1 x)
                     , (tpSigned         , Left $ f2 x)
                     ]

  f1 x = let
    src = decode $ fromStrict $ BS.drop 1 x
    dat = BS.take (1 + 34 + 8) x
    sig = BS.take 64 $ BS.drop (1 + 34 + 8) x in (getUID src, dat, sig)

  f2 msg = let src = decode $ fromStrict $ BS.drop 1 x
               dat = BS.take (len - 64) x
               sig = BS.drop (len - 64) x
           in (getUID src, dat, sig)
  in
  case res' of
    Right x  -> x
    Left (UID pk, dat, sig) -> verify pk sig dat

--msgVerify = const True

msgSign :: ByteString -> ByteString -> ByteString
msgSign sk s = let
  tp = BS.head s
  sign1 = let s1 = BS.take (1 + 34 + 8) s
              s2 = BS.drop (1 + 34 + 8 + 64) s
              s3 = sign sk s1
          in s1 <> s3 <> s2
  sign2 = let s1 = BS.take (BS.length s - 64) s
              s2 = sign sk s1
          in s1 <> s2
  in fromMaybe s $ L.lookup tp [ (tpEnvelopedSignal, sign1)
                               , (tpSigned, sign2)]
--msgSign sk s = s

emptySign = BS.replicate 64 0

-- NOTE: need optimize to avoid put twice
instance Binary Msg where
  put t = case t of
          MsgHeartbeat {}  -> putWord8 tpHeartbeart
          MsgEnvelopedSignal {} -> do putWord8 tpEnvelopedSignal
                                      put $ _msg_src t
                                      Put.putWord64be $ _msg_epoch t
                                      Put.putByteString $ _msg_sign t
                                      put $ _msg_envelope t
          MsgSigned {} -> do putWord8 tpSigned
                             put $ _msg_src t
                             put $ _msg_dst t
                             Put.putWord64be $ _msg_epoch t
                             put $ _msg_payload t
                             Put.putByteString $ _msg_sign t

  get = do tp <- getWord8
           case tp of
             _ | tp == tpHeartbeart -> return $ MsgHeartbeat
             _ | tp == tpEnvelopedSignal ->
                        do src <- get
                           epoch <- Get.getWord64be
                           sign <- Get.getByteString 64
                           evp <- get
                           return MsgEnvelopedSignal { _msg_src = src
                                                     , _msg_epoch = epoch
                                                     , _msg_sign = sign
                                                     , _msg_envelope = evp
                                                     }
             _ | tp == tpSigned -> do
                src <- get
                dst <- get
                epoch <- Get.getWord64be
                payload <- get
                sign <- Get.getByteString 64
                return MsgSigned { _msg_src = src
                                 , _msg_dst = dst
                                 , _msg_epoch = epoch
                                 , _msg_payload = payload
                                 , _msg_sign = sign
                                 }
             _ -> fail $ "Invalid type " ++ show tp

msgFillEpoch :: Msg -> IO Msg
msgFillEpoch msg = case msg of
                     MsgSigned {} ->
                       getEpochMs >>= \e -> return msg { _msg_epoch = e }
                     MsgEnvelopedSignal {} ->
                       getEpochMs >>= \e -> return msg { _msg_epoch = e }
                     _ -> return msg

msgGetSignature :: ByteString -> ByteString
msgGetSignature m =
  let tp = BS.head m
      len = BS.length m in
  case () of
  _ | tp == tpEnvelopedSignal -> BS.take 64 $ BS.drop (1 + 34 + 8) m
  _ | tp == tpSigned -> BS.drop (len - 64) m
  _ -> emptySign

-- TODO, quick check
msgT1 :: ByteString -> Msg -> IO ()
msgT1 sk m = do
  putStrLn "\n\n----"
  putStrLn $ "m = " ++ show m
  let b1 = toStrict $ encode m
  putStrLn $ "enc = " ++ show b1
  putStrLn $ "vfy(F) = " ++ show (msgVerify b1)
  let b2 = msgSign sk b1
  putStrLn $ "sgn = " ++ show b2
  putStrLn $ "vfy = " ++ show (msgVerify b2)
  let m1 = decode $ fromStrict b1 :: Msg
  let m2 = decode $ fromStrict b2 :: Msg
  putStrLn $ "dec = " ++ show m1
  putStrLn $ "eq  = " ++ show (m1 == m)
  putStrLn $ ">>  = " ++ show m2
  putStrLn $ "eq  = " ++ show (decode (encode m2) == m2)

  case m2 of
    MsgEnvelopedSignal {} ->
      putStrLn $ "eq = " ++ show (_msg_sign m2 == msgGetSignature b2)
    MsgSigned {} ->
      putStrLn $ "eq = " ++ show (_msg_sign m2 == msgGetSignature b2)
    _ -> return ()

msgTrivalTest :: NID -> ByteString -> IO ()
msgTrivalTest nid sk = do
  msgT1 sk $ MsgHeartbeat

  t1 <- getEpochMs
  let m1 = MsgEnvelopedSignal nid t1 emptySign (EvpOGM 123)
  msgT1 sk m1

  t2 <- getEpochMs
  let m2 = MsgSigned nid nid0 t2 (MkRTCMsg (MkRTCSignal RTCOffer $ T.pack "abcde"))
                     emptySign
  msgT1 sk m2

  t3 <- getEpochMs
  let m3 = MsgSigned nid nid0 t3 (MkRouteMsg (MkRouteInit [nid]))
                     emptySign
  msgT1 sk m3
