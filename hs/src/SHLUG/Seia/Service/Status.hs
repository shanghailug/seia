{-# Language TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language RecursiveDo  #-}


module SHLUG.Seia.Service.Status (renderStatus) where

import SHLUG.Seia.Conf
import SHLUG.Seia.Rt
import SHLUG.Seia.Type
import SHLUG.Seia.Log

import SHLUG.Seia.Network.MQTT(MQTTState(..))
import SHLUG.Seia.Network.Conn(ConnState(..))

import Reflex
import Reflex.Dom.Core


import Language.Javascript.JSaddle( JSM(..)
                                  , MonadJSM(..)
                                  , liftJSM
                                  , val, fun
                                  , js, jss, jsf
                                  , js0, js1, js2, jsg
                                  , valToNumber, valToText
                                  )

import Control.Monad.Fix (MonadFix(..))
import Control.Lens

import qualified Data.Text as T

{-

<mqtt>

--------
<node> <st> <rt> <last msg>


--------
rtbl
<node> <nei> <last tm> <nei> <last tm>

---
curr time

-}
renderStatus :: ( Reflex t
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
                Dynamic t MQTTState ->
                Event t NID -> Event t (NID, Int) ->
                Event t (NID, ConnState) -> m ()
renderStatus mqttD rxE' rttE stE = do
  -- mqtt state
  dynText $ ffor mqttD $ ("MQTT state: " <>) . T.pack . show

  el "hr" blank

  return ()
