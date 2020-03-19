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

import Data.Time
import Data.Maybe
import Control.Monad.IO.Class (liftIO, MonadIO(..))


import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Text.Printf
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

data EWarp = E_le  NID UTCTime |
             E_rtt NID Int |
             E_cst NID ConnState

data NodeState = MkNodeState { _node_le :: Maybe UTCTime
                             , _node_cst :: ConnState
                             , _node_rtt :: Int
                             }

stRow :: ( Reflex t
         , DomBuilder t m
         , PostBuild t m
         , MonadHold t m
         ) =>
         (NID, NodeState) -> m ()
stRow (nid, st) = do
  el "tr" $ do el "td" $ text (T.pack $ show nid)
               el "td" $ text (T.pack $ show $ _node_cst st)
               elAttr "td" ("style" =: "text-align:right") $
                      text (T.pack $ show $ _node_rtt st)
               el "td" $ text (T.pack $ fromMaybe "" (show <$> (_node_le st)))

stTable rxE' rttE stE = do
  -- TODO, use Incremental

  leE <- performEvent $ ffor rxE' $ \nid -> do
      t <- liftIO getCurrentTime
      return (E_le nid t)

  let esum = leftmost [ ffor stE (\(nid, st) -> E_cst nid st)
                      , ffor rttE (\(nid, rtt) -> E_rtt nid rtt)
                      , leE
                      ]

  let fAcc m = \case
           E_le nid t -> M.update (\x -> Just (x { _node_le = Just t})) nid m
           E_rtt nid rtt -> M.update (\x -> Just (x {_node_rtt = rtt})) nid m
           E_cst nid cst ->
                 case (M.member nid m, connStEnd cst) of
                      (_, True) -> M.update (\x -> Just (x { _node_cst = cst
                                                           , _node_rtt = -1}))
                                            nid m
                      (True, False) -> M.update (\x ->
                                                Just (x {_node_cst = cst})) nid m
                      (False, False) -> M.insert nid (MkNodeState { _node_le = Nothing
                                                                  , _node_cst = cst
                                                                  , _node_rtt = -1
                                                                  }) m


  stD <- accumDyn fAcc M.empty esum

  e <- el "table" $ do
         el "tr" $ do el "th" $ text "node"
                      el "th" $ text "state"
                      el "th" $ text "RTT(ms)"
                      el "th" $ text "last active"
         let a = (mapM_ stRow . M.toList) <$> stD
         b <- dyn a
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
                Event t (NID, ConnState) ->
                Dynamic t Int -> m ()
renderStatus mqttD rxE' rttE stE verD = do
  rtconf <- liftJSM rtConf
  -- current time
  sec1s <- liftIO getCurrentTime >>= tickLossy 1
  el "p" $
     holdDyn "-" (ffor sec1s $
                       ("now: " <>) . T.pack . show . _tickInfo_lastUTC) >>=
                 dynText


  el "p" $
     dynText $ ffor verD $ \v -> T.pack (printf "SEQ: rt %d, curr %d, last %d"
                                        (_rt_seq rtconf)
                                        (_rt_seq_curr rtconf)
                                        v)

  -- mqtt state
  el "p" $
     dynText $ ffor mqttD $ ("MQTT state: " <>) . T.pack . show

  --
  el "p" $ stTable rxE' rttE stE

  return ()
