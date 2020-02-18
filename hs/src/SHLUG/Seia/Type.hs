{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module SHLUG.Seia.Type
  ( nid0
  , getUID, getSID, toNID
  , NID(..), UID(..)
  ) where

-- import Crypto.ECC.Ed25519.Sign
import Data.Binary
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Data.Typeable
import Data.Data
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

import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read(Read(..))

type PubKey = ByteString

newtype UID = UID PubKey deriving (Eq, Ord, Generic, Typeable, Data)
instance Binary UID
instance Show UID where
  show (UID uid) = C8.unpack $ B32.encode uid

instance Read UID where
  readPrec = do
    s <- ReadPrec.lift $ ReadP.count 52 ReadP.get
    let bs = C8.pack s
    case B32.decode bs of
      Right x -> return (UID x)
      Left  x -> fail x

 -- first 32 byte is uid, last 2 byte is sid, big endian
newtype NID = NID ShortByteString deriving (Eq, Ord, Generic, Typeable, Data)

instance Binary NID where
   put t = Put.putByteString $ nid2bin t
   get = do
     bin <- Get.getByteString 34
     return $ bin2nid bin

instance Show NID where
  show nid = show uid ++ ":" ++ show sid
       where uid = getUID nid
             sid = getSID nid

instance Read NID where
  readPrec = do
    uid <- readPrec
    ReadPrec.lift (ReadP.char ':')
    sid <- readPrec
    return $ toNID uid sid

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
