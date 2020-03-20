{-# Language TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language RecursiveDo  #-}
{-# language OverloadedStrings #-}

{-# language PatternSynonyms,
             TypeSynonymInstances,
             FlexibleInstances,
             MultiParamTypeClasses,
             InstanceSigs,
             GeneralizedNewtypeDeriving
#-}


module Main where
import SHLUG.Seia.Conf
import SHLUG.Seia.Rt
import SHLUG.Seia.Type
import SHLUG.Seia.Network.MQTT
import SHLUG.Seia.Msg
import SHLUG.Seia.Network.ConnMan
import SHLUG.Seia.Log
import SHLUG.Seia.Service

import Text.Show.Unicode

import Control.Concurrent (threadDelay, forkIO)

import qualified Data.ByteString.UTF8 as UTF8

import Language.Javascript.JSaddle( askJSM
                                  , JSM(..)
                                  , JSContextRef
                                  , MonadJSM(..)
                                  , liftJSM
                                  , syncPoint
                                  , runJSM
                                  , runJSaddle
                                  , val, fun
                                  , js, jss, jsf
                                  , js0, js1, js2, jsg
                                  , valToNumber, valToText
                                  )
import Control.Lens ((^.))

import Control.Concurrent.MVar
import Data.Time(getCurrentTime)
import qualified System.IO as IO

import Control.Monad (forever, when, join)
import Control.Monad.IO.Class (liftIO, MonadIO(..))

import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Trans.Maybe (MaybeT(..))

import qualified Data.Text as T
import Data.Text (Text(..))

import qualified Data.ByteString as BS

import System.IO (hFlush, stdout)
import qualified Data.Map as M
import Data.Map (Map(..))

import Reflex
import Reflex.Dom.Core

import Reflex.Host.Basic(basicHostForever)

import Control.Lens
import Control.Monad.Catch (MonadCatch, catch) -- JSM is MonadCatch

import Control.Monad.IO.Class (MonadIO)

import Colog(richMessageAction, Msg(..))
import Colog.Core.Action(cfilter)

app :: ( Reflex t
       , TriggerEvent t m
       , PerformEvent t m
       , MonadJSM (Performable m)
       , MonadSample t (Performable m)
       , MonadHold t (Performable m)
       , MonadHold t m
       , MonadJSM m
       , MonadFix m
       , PostBuild t m
       , WithLogIO m
       ) => m (ConnMan t, Dynamic t Int)
app = do
  liftJSM $ rtInit

  logIOM I "app start"
  isnodejs <- liftJSM isNodeJS

  rt_conf <- liftJSM rtConf
  logIOM I $ "rt_conf = " `T.append` (T.pack $ show rt_conf)
  conf <- confB never never
  t <- sample conf
  let t' = t { _conf_priv_key = BS.empty }
  logIOM I $ "conf = " `T.append` (T.pack $ show t')

  let (UID pk) = getUID $ _conf_nid t
  when (not $ _rt_conf_skip_benchmark rt_conf) $
       liftIO $ sign_verify_benchmark (_conf_priv_key t, pk) 1000

  --liftIO $ msgTrivalTest (_conf_nid t) (_conf_priv_key t)
  --mqttLoopbackTest
  let tx = never
  connMan <- connManNew MkConnManConf { _conn_man_tx = tx
                                      , _rt_conf = rt_conf
                                      , _conf = conf
                                      }

  performEvent_ $ ffor (updated $ _conn_man_mqtt_state connMan) $ \ev ->
    logIOM D $ "mqtt state: " `T.append` (T.pack $ show ev)

  verD <- serviceConsole connMan

  performEvent_ $ ffor (updated verD) $ \seq -> do
    logIOM I $ "new seq: " <> T.pack (show seq)
    when (_rt_conf_auto_restart rt_conf) $ do
         logIOM I $ "restarting..."
         liftIO $ threadDelay 1000
         liftJSM jsRestart

  return (connMan, verD)

appDOM :: ( Reflex t
          , DomBuilder t m
          , DomBuilderSpace m ~ GhcjsDomSpace
          , TriggerEvent t m
          , PerformEvent t m
          , MonadJSM (Performable m)
          , MonadSample t (Performable m)
          , MonadHold t (Performable m)
          , MonadHold t m
          , MonadJSM m
          , MonadFix m
          , PostBuild t m
          , WithLogIO m
          ) =>
          (ConnMan t, Dynamic t Int) -> m ()
appDOM (cm, verD) = do
  serviceDOM (cm, verD)

main :: IO ()
main = do
  -- TODO, use askJSM?
  rtconf <- rtConf

  let logEnv = MkLogEnv $ cfilter (\x -> elem (msgSeverity x)
                                              (_rt_conf_log_level rtconf)
                                  ) richMessageAction

  -- JSM is IO
  x <- isNodeJS

  if x
  then basicHostForever $ withLogIO logEnv (app >> return ())
  else mainWidget $ withLogIO logEnv (app >>= appDOM)
  -- for nodejs, not quit
  liftIO $ forever $ threadDelay 5000000
