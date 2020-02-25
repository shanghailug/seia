{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}

module SHLUG.Seia.Msg.Payload where

import GHC.Generics
import Data.Data
import Data.Binary

import SHLUG.Seia.Type
import Data.Text (Text(..))

data Payload = MkRTCMsg RTCMsg |
               MkRouteMsg RouteMsg
     deriving (Eq, Show, Generic, Data)

instance Binary Payload

---------------------- RTC -----------------------
data RTCMsgResType = RTCMsgResExist |
                     RTCMsgResIncompatiable |
                     RTCMsgResOK
                   deriving (Eq, Show, Generic, Data)

instance Binary RTCMsgResType

data RTCSignalType = RTCOffer | RTCAnswer | RTCCandidate
                     deriving (Eq, Show, Generic, Data)
instance Binary RTCSignalType

data RTCMsg =  MkRTCReq Int |
               MkRTCRes RTCMsgResType |
               MkRTCSignal RTCSignalType Text
            deriving (Eq, Show, Generic, Data)

instance Binary RTCMsg


------------------------- route --------------------
data RouteMsg = MkRouteInit [NID] -- init route info
                            deriving (Eq, Show, Generic, Data)

instance Binary RouteMsg
