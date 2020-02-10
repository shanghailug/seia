{-# Language TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language RecursiveDo  #-}

module Main where
import SHLUG.Seia.Conf
import SHLUG.Seia.Rt

import Text.Show.Unicode

import Control.Concurrent (threadDelay, forkIO)

import Crypto.ECC.Ed25519.Sign
import qualified Data.ByteString.UTF8 as UTF8

import Language.Javascript.JSaddle( askJSM
                                  , JSM(..)
                                  , JSContextRef
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

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Location as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.NonElementParentNode as DOM
import qualified GHCJS.DOM.EventTarget as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM

import Control.Monad (forever, when, join)
import Control.Monad.IO.Class (liftIO, MonadIO(..))

import qualified Data.Text as T
import Data.Text (Text(..))

import System.IO (hFlush, stdout)
import qualified Data.Map as M
import Data.Map (Map(..))

import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Location (getLocationPath, getLocationAfterHost)

import Control.Monad.Fix (MonadFix)

import Control.Lens
import Control.Monad.Catch (MonadCatch, catch) -- JSM is MonadCatch


p :: Int -> String -> IO ()
p n x = do
  putStrLn x
  IO.hFlush IO.stdout
  threadDelay $ 1000 * n

app
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js t m
     , MonadFix m
     , PostBuild t m
     , DOM.MonadJSM m
     , PerformEvent t m
     , MonadIO (Performable m)
     , MonadSample t (Performable m)
     ) => m ()
app = do
    liftIO $ putStrLn "app start..."
    liftIO $ forkIO $ mapM_(p 1700 . show) [1..]
    liftIO $ forkIO $ mapM_(p 1300 . (show . (* 1.5))) [1..]
    liftIO $ p 10000 "app init ok" -- print then delay 10sec
    liftIO $ p 1 "reload"
    DOM.liftJSM $ do
      doc <- DOM.currentDocumentUnchecked
      loc <- DOM.getLocationUnsafe doc
      href <- DOM.getHref loc :: JSM String
      liftIO $ putStrLn (show href) >> IO.hFlush IO.stdout
      DOM.reload loc

t_bs :: String -> JSM ()
t_bs s = do
  uprint s
  let a1 = UTF8.fromString s
  let a2 = UTF8.drop 6 a1
  b <- bs_to_u8a a2
  c <- u8a_to_bs b
  uprint $ UTF8.toString c

main :: IO ()
main = do
  putStrLn "currently main is just trival tests"

  e <- storeExist "abcd"
  print ("exist: ", e)

  i <- isNodeJS
  print ("isNodeJS", i)

  g <- storeGet "test/aaa"
  uprint ("get", UTF8.toString <$> g)

  r <- storeSet "test/aaa" $ UTF8.fromString "abcd，中文测试"
  print ("set", r)
  g <- storeGet "test/aaa"
  uprint ("get", UTF8.toString <$> g)

  storeExist "test/aaa" >>= print
  storeRemove "test/aaa" >>= print
  storeExist "test/aaa" >>= print

  mainWidget $ do
    DOM.liftJSM $ t_bs "abcd，测试中文，abcd"
    DOM.liftJSM $ t_bs "ABCD"
    liftIO $ putStrLn "main start..."
    app
