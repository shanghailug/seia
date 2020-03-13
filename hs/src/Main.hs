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
       ) => m ()
app = do
  logIOM I "app start"

  rt_conf <- liftJSM rtConf
  logIOM I $ "rt_conf = " `T.append` (T.pack $ show rt_conf)
  conf <- confB never never
  t <- sample conf
  let t' = t { _conf_priv_key = BS.empty }
  logIOM I $ "conf = " `T.append` (T.pack $ show t')

  --liftIO $ msgTrivalTest (_conf_nid t) (_conf_priv_key t)
  --mqttLoopbackTest
  let tx = never
  connMan <- connManNew MkConnManConf { _conn_man_tx = tx
                                      , _rt_conf = rt_conf
                                      , _conf = conf
                                      }

  performEvent_ $ ffor (updated $ _conn_man_mqtt_state connMan) $ \ev ->
    logIOM D $ "mqtt state: " `T.append` (T.pack $ show ev)

  return ()

main :: IO ()
main = do
  let logEnv1 = logEnvDefault
  let logEnv2 = MkLogEnv $ cfilter (\case Msg { msgSeverity = I }-> True
                                          _ -> False
                                   ) richMessageAction
  let logEnv = if True then logEnv1 else logEnv2
  mainWidget $ withLogIO logEnv app
  -- for nodejs, not quit
  liftIO $ forever $ threadDelay 5000000
