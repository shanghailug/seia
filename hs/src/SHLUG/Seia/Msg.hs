{-# language DeriveGeneric, DeriveDataTypeable #-}
{-# language DuplicateRecordFields #-}

module SHLUG.Seia.Msg
  ( Msg(..)
  , msgType, msgType'
  , msgVerify
  , msgSign
  , emptySign
  , msgTrivalTest
  , msgIsHB, msgIsGeneral, msgIsOGM, msgIsRTC, msgIsRoute
  , msgIsSigned
  , msgFillEpoch
  , msgHB
  ) where

import SHLUG.Seia.Type
import SHLUG.Seia.Helper

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.UTF8 as UTF8

import Data.Binary
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import Data.Typeable
import Data.Data
import GHC.Generics
import Data.Int

import Crypto.ECC.Ed25519.Sign (dverify, dsign)
import Crypto.ECC.Ed25519.Internal.Ed25519(SecKey(..))

--import Test.QuickCheck

data MsgSignType = MsgSignInvalid |
                   MsgSignValid |
                   MsgSign1 |
                   MsgSign2 deriving (Eq, Show)
type MsgType = Char


_mt_invalid = '\0'
_mt_hb      = '\1'
_mt_ogm     = '\2'
_mt_general = '\3'
_mt_rtc     = '\4'
_mt_route   = '\5'

msgSignType :: MsgType -> MsgSignType
msgSignType t | t == _mt_hb                = MsgSignValid
              | t == _mt_ogm               = MsgSign1
              | elem t [_mt_general
                       , _mt_rtc
                       , _mt_route]        = MsgSign2
              | True                       = MsgSignInvalid


data Msg = MsgHB { _msg_type :: !MsgType } | -- type 1
           -- type 2, MsgSign1
           MsgOGM { _msg_type :: !MsgType
                  , _msg_src :: !NID
                  , _msg_epoch :: !Word64
                  , _msg_sign :: !ByteString -- 64 byte
                  -- other runtime data
                  , _msg_hop :: !Word8
                  } |
           -- type 3, 4, 5 MsgSign2
           MsgSigned { _msg_type :: !MsgType
                     , _msg_src :: !NID
                     , _msg_dst :: !NID
                     , _msg_epoch :: !Word64
                     , _msg_payload :: !ByteString
                     , _msg_sign :: !ByteString
                     } |
           MsgInvalid { _msg_type :: !MsgType } -- type = 0
           deriving (Eq, Show, Typeable, Data)

msgHB = MsgHB _mt_hb

msgType' :: ByteString -> (Char, Int)
msgType' s = case UTF8.decode s of
             Nothing     -> (_mt_invalid, -1)
             Just (c, l) -> (c, l)

msgIsHB :: ByteString -> Bool
msgIsHB x = let tp = msgType x in tp == _mt_hb

msgIsGeneral :: ByteString -> Bool
msgIsGeneral x = let tp = msgType x in tp == _mt_general

msgIsRTC :: ByteString -> Bool
msgIsRTC x = msgType x == _mt_rtc

msgIsOGM :: ByteString -> Bool
msgIsOGM x = msgType x == _mt_ogm

msgIsRoute :: ByteString -> Bool
msgIsRoute x = msgType x == _mt_route

msgIsSigned :: ByteString -> Bool
msgIsSigned x = let st = msgSignType (msgType x) in st == MsgSign2

msgType :: ByteString -> Char
msgType = fst . msgType'

msgVerify :: ByteString -> Bool
msgVerify x = let
  (tp, clen) = msgType' x
  len = BS.length x

  res' = case msgSignType tp of
    MsgSignValid -> Right True
    MsgSign1     -> Left $ f1 x
    MsgSign2     -> Left $ f2 x
    _            -> Right False

  f1 x = let
    src = decode $ BS.Lazy.fromStrict $ BS.drop clen x
    dat = BS.take (clen + 34 + 8) x
    sig = BS.take 64 $ BS.drop (clen + 34 + 8) x in (getUID src, dat, sig)

  f2 msg = let src = decode $ BS.Lazy.fromStrict $ BS.drop clen x
               dat = BS.take (len - 64) x
               sig = BS.drop (len - 64) x
           in (getUID src, dat, sig)
  in
  case res' of
    Right x  -> x
    Left (UID pk, dat, sig) ->
         case dverify pk sig dat of
         Right _ -> True
         Left  _ -> False


msgSign :: ByteString -> ByteString -> ByteString
msgSign sk s = let
  (tp, clen) = msgType' s
  key = SecKeyBytes sk
  sign1 = let s1 = BS.take (clen + 34 + 8) s
              s2 = BS.drop (clen + 34 + 8 + 64) s
              Right s3 = dsign key s1
          in s1 <> s3 <> s2
  sign2 = let s1 = BS.take (BS.length s - 64) s
              Right s2 = dsign key s1
          in s1 <> s2
  in case msgSignType tp of
     MsgSign1 -> sign1
     MsgSign2 -> sign2
     _ -> s

emptySign = BS.replicate 64 0

-- NOTE: need optimize to avoid put twice
instance Binary Msg where
  put t = case t of
          MsgHB {}  -> put $ _msg_type t
          MsgOGM {} -> do put $ _msg_type t
                          put $ _msg_src t
                          Put.putWord64be $ _msg_epoch t
                          Put.putByteString $ _msg_sign t
                          Put.putWord8 $ _msg_hop t
          MsgSigned {} -> do put $ _msg_type t
                             put $ _msg_src t
                             put $ _msg_dst t
                             Put.putWord64be $ _msg_epoch t
                             Put.putByteString $ _msg_payload t
                             Put.putByteString $ _msg_sign t
          _ -> put _mt_invalid

  get = do tp <- get
           case tp of
             _ | tp == _mt_hb -> return $ MsgHB tp
             _ | tp == _mt_ogm ->
                        do src <- get
                           epoch <- Get.getWord64be
                           sign <- Get.getByteString 64
                           hop <- Get.getWord8
                           return MsgOGM { _msg_type = tp
                                         , _msg_src = src
                                         , _msg_epoch = epoch
                                         , _msg_sign = sign
                                         , _msg_hop = hop
                                         }
             _ | elem tp [_mt_general, _mt_rtc, _mt_route] -> do
                src <- get
                dst <- get
                epoch <- Get.getWord64be
                remain <- BS.Lazy.toStrict <$> Get.getRemainingLazyByteString
                let plen = BS.length remain - 64
                let payload = BS.take plen remain
                let sign = BS.drop plen remain
                return MsgSigned { _msg_type = tp
                                 , _msg_src = src
                                 , _msg_dst = dst
                                 , _msg_epoch = epoch
                                 , _msg_payload = payload
                                 , _msg_sign = sign
                                 }
             _ -> return $ MsgInvalid _mt_invalid

-- TODO, quick check
msgT1 :: ByteString -> Msg -> IO ()
msgT1 sk m = do
  putStrLn "\n\n----"
  putStrLn $ "m = " ++ show m
  let b1 = BS.Lazy.toStrict $ encode m
  putStrLn $ "enc = " ++ show b1
  putStrLn $ "vfy = " ++ show (msgVerify b1)
  let b2 = msgSign sk b1
  putStrLn $ "sgn = " ++ show b2
  putStrLn $ "vfy = " ++ show (msgVerify b2)
  let m1 = decode $ BS.Lazy.fromStrict b1 :: Msg
  let m2 = decode $ BS.Lazy.fromStrict b2 :: Msg
  putStrLn $ "dec = " ++ show m1
  putStrLn $ "eq  = " ++ show (m1 == m)
  putStrLn $ ">>  = " ++ show m2
  putStrLn $ "eq  = " ++ show (decode (encode m2) == m2)

msgFillEpoch :: Msg -> IO Msg
msgFillEpoch msg = case msg of
                     MsgSigned {} ->
                       getEpochMs >>= \e -> return msg { _msg_epoch = e }
                     MsgOGM {} ->
                       getEpochMs >>= \e -> return msg { _msg_epoch = e }
                     _ -> return msg

msgTrivalTest :: NID -> ByteString -> IO ()
msgTrivalTest nid sk = do
  msgT1 sk $ MsgHB '\1'

  t1 <- getEpochMs
  let m1 = MsgOGM _mt_ogm nid t1 emptySign 123
  msgT1 sk m1

  t2 <- getEpochMs
  let m2 = MsgSigned _mt_general nid nid0 t2 (BS.Lazy.toStrict $ encode m1)
                     emptySign
  msgT1 sk m2

  t3 <- getEpochMs
  let m3 = MsgSigned _mt_rtc nid nid0 t3 (BS.Lazy.toStrict $ encode m2)
                     emptySign
  msgT1 sk m3
