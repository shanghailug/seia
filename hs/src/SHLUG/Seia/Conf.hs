{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SHLUG.Seia.Conf ( confB
                       , confGet
                       , confSet
                       , confRm
                       , confConst
                       , ConfConst(..)
                       , Conf(..)
                       ) where

import SHLUG.Seia.Rt
import SHLUG.Seia.Type

import Language.Javascript.JSaddle ( JSM(..)
                                   , MonadJSM(..)
                                   , liftJSM
                                   )

import Data.Text ( Text(..) )
import qualified Data.Text as T

import Data.ByteString(ByteString(..))
import qualified Data.ByteString as BS

import Reflex
import Control.Monad.Fix (MonadFix)

import Control.Monad.Catch (MonadCatch, catch)
import Control.Exception (SomeException)

import Data.Set ( Set(..) )
import qualified Data.Set as Set

import Data.Map.Strict ( Map(..) )
import qualified Data.Map.Strict as Map

import qualified Data.Binary as Bin
import Data.Binary (Binary(..))

import Data.Maybe (fromJust)

import Crypto.ECC.Ed25519.Sign
import Crypto.ECC.Ed25519.Internal.Ed25519(SecKey(..))

import System.Random ( randomRIO )

import Control.Monad.IO.Class ( MonadIO(..), liftIO)

import Data.ByteString.Lazy (fromStrict, toStrict)
{-
every service save data under key "<serivce>/*"
and conf of service store config under "conf/<service>/*"
conf itself will store config under "conf/conf"
-}

confGet :: Text -> Text -> JSM (Maybe ByteString)
confGet p s = storeGet (p <> T.pack "/" <> s)

confSet :: Text -> Text -> ByteString -> JSM Bool
confSet p s v = storeSet (p <> T.pack "/" <> s) v

confRm :: Text -> Text -> JSM Bool
confRm p s = storeRemove (p <> T.pack "/" <> s)

_c_PREFIX   = "conf"
_c_KEYPAIR = "kp"
_c_TURN_SERVER = "turn_server"
_c_BOOTSTRAP_NODE = "bootstrap_node"

initKP :: JSM (UID, ByteString)
initKP = do
  kp <- confGet _c_PREFIX _c_KEYPAIR
  (sk, pk) <- case kp of
                   Just x  -> return $ Bin.decode $ fromStrict x
                   Nothing -> do Right (SecKeyBytes x, y) <- liftIO genkeys
                                 confSet _c_PREFIX _c_KEYPAIR $
                                         toStrict $ Bin.encode (x, y)
                                 (Bin.decode . fromStrict . fromJust) <$>
                                             confGet _c_PREFIX _c_KEYPAIR
  return (UID pk, sk)

init' :: Binary a => Text -> a -> JSM a
init' key val0 = do
  x <- confGet _c_PREFIX key
  case x of
    Just y  -> return $ Bin.decode $ fromStrict y
    Nothing -> do confSet _c_PREFIX key $ toStrict $ Bin.encode val0
                  (Bin.decode . fromStrict . fromJust) <$> confGet _c_PREFIX key

-- config for conf itsel or global conf
data Conf = Conf { _conf_turn_server :: [Text]
                 , _conf_bootstrap_node :: [NID]
                 , _conf_nid :: NID
                 , _conf_priv_key :: ByteString
                 } deriving (Eq, Show)

confB :: ( Reflex t
            , PerformEvent t m
            , MonadIO (Performable m)
            , MonadHold t m
            , MonadFix m
            , MonadJSM m) =>
            Event t [Text] -> Event t [NID] ->
            m (Behavior t Conf)
confB evSetTurnServer evSetBootstrapNode = do
  rt_conf <- liftJSM rtConf
  -- get key pair
  (uid, sk) <- liftJSM initKP
  sid <- case _rt_sid rt_conf of
         Nothing -> liftIO $ randomRIO (0x100, 0xffff)
         Just x  -> return x
  let nid = toNID uid sid

  ts <- liftJSM $ init' _c_TURN_SERVER []
  bn <- liftJSM $ init' _c_BOOTSTRAP_NODE []

  let ts' = ts ++ _rt_conf_turn_server rt_conf
  let bn' = bn ++ _rt_conf_fallback_bootstrap_node rt_conf

  -- if sk is actually secret seed,
  -- then we generate 64 byte secret key from seed
  let sk' = if BS.length sk == 32 then seed2sk sk else sk

  let res0 = Conf { _conf_turn_server = ts'
                  , _conf_bootstrap_node = bn'
                  , _conf_nid = nid
                  , _conf_priv_key = sk'
                  }

  let e1 = ffor evSetTurnServer    Left
  let e2 = ffor evSetBootstrapNode Right

  let ev = fmap Map.elems $ mergeMap $ Map.fromList [(1, e1), (2, e2)]

  let f acc e = case e of
                Left ts -> acc { _conf_turn_server = ts }
                Right bn -> acc { _conf_bootstrap_node = bn }

  performEvent_ $ ffor evSetTurnServer $ \s -> do
    liftJSM $ confSet _c_PREFIX _c_TURN_SERVER $ toStrict $ Bin.encode s
    return ()

  performEvent_ $ ffor evSetBootstrapNode $ \b -> do
    liftJSM $ confSet _c_PREFIX _c_BOOTSTRAP_NODE $ toStrict $ Bin.encode b
    return ()

  accumB (foldl f) res0 ev


data ConfConst = MkConfConst { _cc_conn_req_timeout :: Int
                             , _cc_conn_signal_timeout :: Int
                             , _cc_conn_heartbeat_timeout :: Double
                             , _cc_cm_ogm_interval :: Int
                             , _cc_cm_ogm_timeout :: Int
                             , _cc_mqtt_heartbeat_timeout  :: Int
                             , _cc_mqtt_heartbeat_interval :: Int
                             }

confConst = MkConfConst { _cc_conn_req_timeout       = 10
                        , _cc_conn_signal_timeout    = 30
                        , _cc_conn_heartbeat_timeout = 6.0 -- 2.0 sec
                        , _cc_cm_ogm_interval        = 20
                        , _cc_cm_ogm_timeout         = 25 -- 5 + _cc_cm_ogm_interval
                        , _cc_mqtt_heartbeat_interval = 10
                        , _cc_mqtt_heartbeat_timeout = 15
                        }
