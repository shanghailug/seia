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
import SHLUG.Seia.Network.Conn(ConnState(..), connStEnd)

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
import qualified Data.Map.Strict as M

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

filterNID :: (Reflex t) => NID -> Event t (NID, a) -> Event t a
filterNID nid e = fmap snd $ ffilter ((== nid) . fst) e

stRow :: ( Reflex t
         , DomBuilder t m
         , PostBuild t m
         , MonadHold t m
         ) =>
         (NID, (Int, ConnState)) -> m ()
stRow (nid, (rttM, stM)) = do
  rttD <- fmap (T.pack . show) <$> rttM
  stD  <- fmap (T.pack . show) <$> stM

  --holdDyn "" (fmap (T.pack . show) $ filterNID nid stE)
  --rttDyn <- holdDyn "" (fmap (T.pack . show) $ filterNID nid rttE)

  el "tr" $ do el "td" $ text (T.pack $ show nid)
               el "td" $ dynText stD
               elAttr "td" ("style" := "align:right") $ dynText rttD
               el "td" $ text "--"

stTable rxE' rttE stE = do
  -- Dynamic t [NID]
  let f nid = ( holdDyn (-1) $ filterNID nid rttE
              , holdDyn ConnIdle $ filterNID nid stE)
  stD <- accumDyn (\m (nid, e) -> if connStEnd e
                                  then M.delete nid m
                                  else if M.member nid m then m
                                       else M.insert nid (f nid) m
                  ) M.empty stE

  e <- el "table" $ do
         el "tr" $ do el "th" $ text "node"
                      el "th" $ text "state"
                      el "th" $ text "RTT(ms)"
                      el "th" $ text "last active"
         -- Dynamic [
         let a = (mapM_ stRow . M.toList) <$> stD
         b <- dyn a
         --c <- switchHold never b -- Event t ()
         return ()

  return ()

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

  --
  stTable rxE' rttE stE

  return ()
