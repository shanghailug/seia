{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module SHLUG.Seia.Type
  ( nid0
  , MsgMeta1(..)
  , getUID, getSID, toNID
  , NID(..), UID(..)
  ) where

-- import Crypto.ECC.Ed25519.Sign
import Data.Binary
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Data.Typeable
import GHC.Generics
import Data.Int

import Data.ByteString.Short(ShortByteString(..))
import qualified Data.ByteString.Short as BSS
import Data.ByteString(ByteString(..))
import qualified Data.ByteString as BS

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base32.Z as B32
import qualified Data.ByteString.Lazy as BS.Lazy
import Data.Char (chr, ord)

type PubKey = ByteString

newtype UID = UID PubKey deriving (Eq, Ord, Generic)
instance Binary UID
instance Show UID where
  show (UID uid) = C8.unpack $ B32.encode uid

 -- first 32 byte is uid, last 2 byte is sid, big endian
newtype NID = NID ShortByteString deriving (Eq, Ord, Generic)

instance Binary NID

instance Show NID where
  show nid = show uid ++ ":" ++ show sid
       where uid = getUID nid
             sid = getSID nid

nid0 :: NID
nid0 = NID $ BSS.pack $ replicate 34 0

getUID :: NID -> UID
getUID (NID nid) = UID $ BS.take 32 $ BSS.fromShort nid

getSID :: NID -> Word16
getSID (NID nid) = (fromIntegral $ BSS.index nid 32) * 256 +
                 (fromIntegral $ BSS.index nid 33)

toNID :: UID -> Word16 -> NID
toNID (UID uid) sid = NID $ BSS.toShort $
                      BS.append uid $ BS.pack $
                      map fromIntegral $ [sid `div` 256, sid `mod` 256]

nid2bin :: NID -> ByteString
nid2bin (NID nid) = BSS.fromShort nid

bin2nid :: ByteString -> NID
bin2nid bs = NID $ BSS.toShort bs

-- by order
-- ver:1, kind:1, src: nid:(32+2), dst: nid:(32+2), epoch:8, payload(N), signature: 64

data MsgMeta1 = MkMsgMeta1 { ver :: Word8
                           , kind :: Char -- utf8 char, 1~4 byte
                           , src :: NID
                           , dst :: NID
                           , epoch :: Word64 -- in ms
                           } deriving (Show, Eq, Ord)

instance Binary MsgMeta1 where
  put t = do
      Put.putWord8 $ ver t
      put $ kind t
      Put.putByteString $ nid2bin $ src t
      Put.putByteString $ nid2bin $ dst t
      Put.putWord64be $ epoch t

  get = do
      v <- Get.getWord8
      k <- get
      s <- bin2nid <$> Get.getByteString 34
      d <- bin2nid <$> Get.getByteString 34
      e <- Get.getWord64be
      return $ MkMsgMeta1 { ver = v, kind = k, src = s, dst = d, epoch = e }
