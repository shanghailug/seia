{-# Language TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language RecursiveDo  #-}
{-# language OverloadedStrings #-}

module Main where
import SHLUG.Seia.Conf
import SHLUG.Seia.Rt
import SHLUG.Seia.Type
import SHLUG.Seia.Network.MQTT
import SHLUG.Seia.Msg

import Text.Show.Unicode

import Control.Concurrent (threadDelay, forkIO)

import Crypto.ECC.Ed25519.Sign
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

import qualified Data.Text as T
import Data.Text (Text(..))

import System.IO (hFlush, stdout)
import qualified Data.Map as M
import Data.Map (Map(..))

import Reflex
import Reflex.Dom.Core

import Control.Lens
import Control.Monad.Catch (MonadCatch, catch) -- JSM is MonadCatch

app :: ( Reflex t
       , TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       , MonadHold t m
       , MonadJSM m
       , MonadFix m
       ) => m ()
app = do
  liftIO $ putStrLn "start"
  rt_conf <- liftJSM rtConf
  liftIO $ putStrLn $ "rt_conf = " ++ show rt_conf
  conf <- confB never never
  t <- sample conf

  liftIO $ putStrLn $ "conf = " ++ show t

  liftIO $ msgTrivalTest (_conf_nid t) (_conf_priv_key t)

  mqttLoopbackTest

main :: IO ()
main = do
  mainWidget app
  -- for nodejs, not quit
  liftIO $ forever $ threadDelay 5000000
