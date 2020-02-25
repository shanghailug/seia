{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}

module SHLUG.Seia.Msg.Envelope where

import SHLUG.Seia.Type
import Data.Binary
import GHC.Generics
import Data.Data

data Envelope = EvpOGM { _evp_ogm_hop :: Word8 }
     deriving (Eq, Show, Generic, Data)

instance Binary Envelope
