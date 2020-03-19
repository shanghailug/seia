{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
module SHLUG.Seia.Service ( serviceConsole
                          , serviceDOM
                          ) where

import SHLUG.Seia.Service.Status
import SHLUG.Seia.Service.Version

import SHLUG.Seia.Rt
import SHLUG.Seia.Log
import SHLUG.Seia.Network.ConnMan

import Reflex
import Reflex.Dom.Core


import Language.Javascript.JSaddle( JSM(..)
                                  , MonadJSM(..)
                                  , liftJSM
                                  )
import Control.Monad.Fix (MonadFix(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..), liftIO)

serviceConsole :: ( Reflex t
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
                  ConnMan t -> m (Dynamic t Int)
serviceConsole cm = do
  rtconf <- liftJSM rtConf

  verD <- versionRun
  --
  return verD

serviceDOM :: ( Reflex t
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
serviceDOM (cm, verD) = do
  rtconf <- liftJSM rtConf
  let service = _rt_conf_service rtconf

  --liftIO $ print service
  when (elem "status" service) $ do
       --liftIO $ putStrLn "status start"
       (renderStatus (_conn_man_mqtt_state cm)
                     (_conn_man_rx' cm)
                     (_conn_man_rtt cm)
                     (_conn_man_st_e cm)
                     verD)

  return ()
