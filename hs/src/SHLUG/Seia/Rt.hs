{-# language ForeignFunctionInterface, JavaScriptFFI #-}
module SHLUG.Seia.Rt ( isNodeJS
                     , bs_to_u8a
                     , u8a_to_bs
                     , storeGet
                     , storeSet
                     , storeRemove
                     , storeExist ) where

import Language.Javascript.JSaddle ( JSM(..)
                                   , JSVal(..)
                                   , JSString, fromJSString
                                   , fromJSVal
                                   , ToJSVal, toJSVal
                                   , isNull
                                   , toJSVal
                                   , ghcjsPure
                                   , jsval
                                   , js, jss, jsf
                                   , js0, js1, js2, jsg, jsg1
                                   )

import GHCJS.Buffer ( toByteString, fromByteString
                    , getUint8Array
                    , createFromArrayBuffer
                    )
import GHCJS.Buffer.Types( SomeBuffer(..) )

import JavaScript.TypedArray ( TypedArray(..)
                             , Uint8Array
                             , buffer, byteLength, byteOffset
                             , subarray
                             )
import qualified Data.JSString as JSString

import Data.ByteString(ByteString(..))
import qualified Data.ByteString as BS

import Control.Lens ((^.))


foreign import javascript interruptible
  "window._rt.store.get($1, function (err, res) { $c(err, res); });"
  store_get_a :: JSString -> IO (JSVal, Uint8Array) -- IO (TypeOfErr, Uint8Array)

foreign import javascript interruptible
  "window._rt.store.set($1, $2, $c);"
  store_set_a :: JSString -> Uint8Array -> IO JSVal -- IO (TypeOfErr)

foreign import javascript interruptible
  "window._rt.store.exist($1, $c);"
  store_exist :: JSString -> IO JSVal -- IO Bool

foreign import javascript interruptible
  "window._rt.store.remove($1, $c);"
  store_remove :: JSString -> IO JSVal -- IO Bool

foreign import javascript unsafe "new Uint8Array(new ArrayBuffer(0))"
  js_empty_u8a :: IO Uint8Array

foreign import javascript unsafe "typeof(window.cwd)"
  js_window_cwd_type :: IO JSString

consoleLog :: ToJSVal a => a -> JSM ()
-- TODO, console is undefined, why?
consoleLog a = do
  console <- jsg "console"
  console ^. js1 "log" a
  return ()

u8a_to_bs :: Uint8Array -> JSM ByteString
u8a_to_bs v = do
  let a = v

  --print "u8a_to_bs"

  b <- ghcjsPure (buffer a) >>= (ghcjsPure . createFromArrayBuffer)
  o <- ghcjsPure $ byteOffset a
  l <- ghcjsPure $ byteLength a

  ghcjsPure $ toByteString o (Just l) b

bs_to_u8a :: ByteString -> JSM Uint8Array
bs_to_u8a bs = do
  (buf, off, len) <- ghcjsPure (fromByteString bs) --
  --print (off, len)
  buf' <- ghcjsPure $ getUint8Array buf
  res <- ghcjsPure $ subarray off (off + len) buf'

  if len == 0 then js_empty_u8a else return res

isNodeJS :: JSM Bool
isNodeJS = do
  tp <- js_window_cwd_type
  return $ fromJSString tp == "string"

storeGet :: String -> JSM (Maybe ByteString)
storeGet k = do
  let k' = JSString.pack k
  (err, res) <- store_get_a k'
  noErr <- ghcjsPure $ isNull err
  if noErr then Just <$> u8a_to_bs res else return Nothing

storeSet :: String -> ByteString -> JSM Bool
storeSet k v = do
  let k' = JSString.pack k
  v' <- bs_to_u8a v
  err <- store_set_a k' v'
  ghcjsPure $ isNull err

storeExist :: String -> JSM Bool
storeExist k = do
  let k' = JSString.pack k
  r <- store_exist k'
  r' <- fromJSVal r

  return $ r' == Just True

storeRemove :: String -> JSM Bool
storeRemove k = do
  let k' = JSString.pack k
  r <- store_remove k'
  r' <- fromJSVal r

  return $ r' == Just True
