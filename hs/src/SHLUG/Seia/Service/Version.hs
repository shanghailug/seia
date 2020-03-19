{-# language FlexibleContexts #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language TupleSections #-}
{-# language OverloadedStrings #-}

module SHLUG.Seia.Service.Version (versionRun) where

{-
every 10 sec
1. if _rt_seq not is store download _rt_seq
2. if seia-{_rt_seq + 1} exist, download and save
-}

import SHLUG.Seia.Rt
import SHLUG.Seia.Log

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.XMLHttpRequest as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Enums as Enums

import Data.Text.Encoding(encodeUtf8)

import Reflex

import Language.Javascript.JSaddle ( JSM(..), MonadJSM(..)
                                   , liftJSM
                                   , askJSM, runJSM
                                   , JSContextRef
                                   , JSException(..)
                                   , JSVal(..), toJSVal
                                   , toJSString
                                   , strToText, valToStr, valIsNull
                                   , js1, js
                                   , obj, new
                                   , (<#)
                                   )


import Data.IORef
import qualified Data.Text as T
import Data.Text (Text(..))
import Control.Lens ((^.))

import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad (when)
import Data.Time
import Text.Printf

import qualified System.IO as IO
import Control.Concurrent (forkIO)


reqSetup :: DOM.XMLHttpRequest -> (Maybe Text -> JSM ()) -> JSM ()
reqSetup req cb = do
   -- setup event handle
   DOM.on req DOM.loadEnd $ do
     st  <- liftJSM $ DOM.getStatus req
     txt <- liftJSM $ DOM.getResponseText req
     if st == 200 then liftJSM $ cb txt
                  else liftJSM $ cb Nothing
     return ()

   DOM.on req DOM.timeout $ liftJSM $ cb Nothing

   -- setup timeout
   -- NOTE: 1000 is 1 sec, not 1000 sec
   DOM.setTimeout req $ (1000 * 600)

   -- setup header, TODO, for gz compress
   return ()

reqOpen :: DOM.XMLHttpRequest -> Text -> JSM ()
reqOpen req url = do
  DOM.openSimple req ("GET" :: Text) url
  ctx <- askJSM
  -- "DOM.send req" will block, why?
  liftIO $ forkIO $ runJSM (DOM.send req) ctx
  return ()

xhr :: ( Reflex t
       , TriggerEvent t m
       , MonadSample t m
       , PerformEvent t m
       , MonadFix m
       , MonadHold t m
       , MonadIO m
       , MonadSample t (Performable m)
       , MonadHold t (Performable m)
       , MonadJSM (Performable m)
       , PostBuild t m
       , WithLogIO m
       ) =>
       a -> Event t (a, Text) -> m (Event t (a, Maybe Text), Dynamic t Bool)
xhr tag0 urlE = do
  -- NOTE: when busy, input will drop
  (busyE, busyT) <- newTriggerEvent
  (resE, resT) <- newTriggerEvent

  busyD <- holdDyn False busyE
  let e' = gate (not <$> current busyD) urlE

  tagRef <- liftIO $ newIORef tag0

  req <- liftJSM $ new (js_rt ^. js ("XMLHttpRequest" :: Text)) () >>=
                   (return . DOM.XMLHttpRequest)

  liftJSM $ reqSetup req (\x -> liftIO $ do tag <- readIORef tagRef
                                            resT (tag, x)
                                            busyT False
                         )

  performEvent_ $ ffor e' $ \(tag, url) -> do
    let a = T.isPrefixOf "file://" url
    when (not a) $ do logIOM D $ T.pack $ printf "version: try %s" url
                      liftIO (atomicWriteIORef tagRef tag)
                      liftIO (busyT True)
                      liftJSM (reqOpen req url)

  return (resE, busyD)

versionRun :: ( Reflex t
              , TriggerEvent t m
              , MonadSample t m
              , PerformEvent t m
              , MonadFix m
              , MonadHold t m
              , MonadIO m
              , MonadSample t (Performable m)
              , MonadHold t (Performable m)
              , MonadJSM (Performable m)
              , PostBuild t m
              , WithLogIO m
              ) =>
              m (Dynamic t Int)
versionRun = do
  rtconf <- liftJSM rtConf
  let seq = _rt_seq rtconf
  let purl = _rt_preloader_url rtconf

  let gen_url x = T.intercalate "/" $ init (T.splitOn "/" purl) ++
                                      ["seia-" <> T.pack (show x) <> ".js"]

  (seqE, seqT) <- newTriggerEvent
  (urlE, urlT) <- newTriggerEvent

  (resE, busyD) <- xhr (_rt_seq_curr rtconf) urlE

  tick10s <- liftIO getCurrentTime >>= tickLossy 10
  let tick10s' = gate (not <$> current busyD) tick10s
  performEvent_ $ ffor tick10s' $ \_ -> liftIO $ do
    a <- storeExist $ "version/seia-" <> T.pack (show seq)
    b <- storeExist $ "version/seia-" <> T.pack (show $ seq + 1)

    case (a, b) of
         (False, _)    -> urlT (seq    , gen_url seq)
         (True, False) -> urlT (seq + 1, gen_url $ seq + 1)
         (True, True)  -> liftIO (seqT (seq + 1))

  performEvent_ $ ffor resE $ \(tag, res) -> do
    case res of
      Just x -> do logIOM D $ T.pack $ printf "version: get 'seia-%d'" tag
                   liftJSM $ storeSet ("version/seia-" <> T.pack (show tag))
                                      (encodeUtf8 x)
                   liftIO (seqT tag)
                   return ()
      _ -> return ()

  holdDyn (_rt_seq_curr rtconf) seqE
