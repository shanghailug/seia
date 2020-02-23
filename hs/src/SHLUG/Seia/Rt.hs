{-# language ForeignFunctionInterface, JavaScriptFFI #-}

module SHLUG.Seia.Rt ( isNodeJS
                     , consoleLog
                     , bs_to_u8a
                     , u8a_to_bs
                     , jsval_to_bs
                     , u8a_to_jsval
                     , rtConf
                     , RtConf(..)
                     , storeGet
                     , storeSet
                     , storeRemove
                     , storeExist ) where

import SHLUG.Seia.Type

import Language.Javascript.JSaddle ( JSM(..)
                                   , JSVal(..)
                                   , JSString, fromJSString, toJSString
                                   , fromJSVal
                                   , ToJSVal, toJSVal
                                   , isNull
                                   , toJSVal
                                   , ghcjsPure
                                   , jsval
                                   , js, jss, jsf
                                   , js0, js1, js2, jsg, jsg1
                                   )

import Control.Monad.IO.Class (liftIO)

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

import JavaScript.TypedArray.ArrayBuffer ( ArrayBuffer(..))

import qualified Data.JSString as JSString

import Data.ByteString(ByteString(..))
import qualified Data.ByteString as BS

import Control.Lens ((^.))

import Data.Text ( Text(..) )
import qualified Data.Text as T
import Data.Word ( Word16 )

mainVersion :: Int
mainVersion = 1

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
  store_remove :: JSString -> IO JSVal -- IO TypeOfErr

foreign import javascript unsafe "new Uint8Array(new ArrayBuffer(0))"
  js_empty_u8a :: IO Uint8Array

foreign import javascript unsafe "window._rt.is_nodejs()"
  js_window_rt_is_nodejs :: IO Bool

foreign import javascript unsafe "window._rt.VERSION"
  js_rt_version :: IO Int

foreign import javascript unsafe "window._rt.preloader_url"
  js_rt_preloader_url :: IO JSString

foreign import javascript unsafe
  "if (typeof(window._rt.sid) == 'number') { $r = window._rt.sid; } else { $r = -1; }"
  js_rt_sid :: IO Int

foreign import javascript unsafe "$r = $1;"
  u8a_to_jsval :: Uint8Array -> IO JSVal

foreign import javascript unsafe "$r = $1;"
  jsval_unsafe_cast_to_ab :: JSVal -> IO ArrayBuffer

foreign import javascript unsafe "$r = $1;"
  jsval_unsafe_cast_to_u8a :: JSVal -> IO Uint8Array

foreign import javascript unsafe "$1 instanceof Uint8Array"
  jsval_is_u8a :: JSVal -> IO Bool

foreign import javascript unsafe "$1 instanceof ArrayBuffer"
  jsval_is_ab :: JSVal -> IO Bool

foreign import javascript unsafe "$1 instanceof Blob"
  jsval_is_blob :: JSVal -> IO Bool

foreign import javascript interruptible
  "$1.arrayBuffer().then($c);"
  js_blob_to_ab :: JSVal -> IO ArrayBuffer


{-

global :: Object
#ifdef ghcjs_HOST_OS
global = js_window
foreign import javascript unsafe "$r = window"
    js_window :: Object
#else
global = Object . JSVal . unsafePerformIO $ newIORef 4
#endif
-}

consoleLog :: ToJSVal a => a -> JSM ()
consoleLog a = do
  -- NOTE: jsg will get object from 'window' object
  console <- jsg "console"
  console ^. js1 "log" a
  return ()

ab_to_bs :: ArrayBuffer -> JSM ByteString
ab_to_bs a = do
  b <- ghcjsPure $ createFromArrayBuffer a
  ghcjsPure $ toByteString 0 Nothing b

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

  if len == 0 then liftIO js_empty_u8a else return res

jsval_to_u8a :: JSVal -> JSM (Maybe Uint8Array)
jsval_to_u8a v = do
  x <- liftIO $ jsval_is_u8a v
  if x then Just <$> liftIO (jsval_unsafe_cast_to_u8a v)
       else return Nothing

jsval_to_bs :: JSVal -> JSM (Maybe ByteString)
jsval_to_bs v = do
  x <- liftIO $ jsval_is_u8a v
  y <- liftIO $ jsval_is_ab v
  case (x, y)  of
    (True, False) -> do
      a <- liftIO $ jsval_unsafe_cast_to_u8a v
      Just <$> u8a_to_bs a
    (False, True) -> do
      a <- liftIO $ jsval_unsafe_cast_to_ab v
      Just <$> ab_to_bs a
    -- NOTE: Blob is not exist for nodejs
    _ -> do z <- liftIO $ jsval_is_blob v
            if z then js_blob_to_ab v >>= ab_to_bs >>= (return . Just)
            else return Nothing

isNodeJS :: JSM Bool
isNodeJS = do
  res <- liftIO js_window_rt_is_nodejs
  return res

storeGet :: Text -> JSM (Maybe ByteString)
storeGet k = do
  let k' = toJSString k
  (err, res) <- liftIO $ store_get_a k'
  noErr <- ghcjsPure $ isNull err
  if noErr then Just <$> u8a_to_bs res else return Nothing

storeSet :: Text -> ByteString -> JSM Bool
storeSet k v = do
  let k' = toJSString k
  v' <- bs_to_u8a v
  err <- liftIO $ store_set_a k' v'
  ghcjsPure $ isNull err

storeExist :: Text -> JSM Bool
storeExist k = do
  let k' = toJSString k
  r <- liftIO $ store_exist k'
  r' <- fromJSVal r

  return $ r' == Just True

storeRemove :: Text -> JSM Bool
storeRemove k = do
  let k' = toJSString k
  err <- liftIO $ store_remove k'

  ghcjsPure $ isNull err

data RtConf = RtConf
            { _rt_is_nodejs :: Bool
            , _rt_sid :: Maybe Word16
            , _rt_preloader_url :: Text
            , _rt_main_version :: Int
            , _rt_version :: Int
            -- from windw._rt.conf
            , _rt_conf_turn_server :: [Text]
            , _rt_conf_fallback_bootstrap_node :: [NID]
            } deriving (Eq, Show)

rtConf :: JSM RtConf
rtConf = do
  is_nodejs <- liftIO isNodeJS
  sid' <- liftIO $ js_rt_sid
  ver <- liftIO js_rt_version
  url <- liftIO js_rt_preloader_url
  --turn_server_list <- jsg "window" ^. js "_rt" ^. js "conf" ^. js "turn_server_list
  return $ RtConf { _rt_is_nodejs = is_nodejs
                  , _rt_sid = if sid' < 0 then Nothing else Just (toEnum sid')
                  , _rt_preloader_url = fromJSString url
                  , _rt_main_version = mainVersion
                  , _rt_version = ver
                  --
                  , _rt_conf_turn_server = [] -- TODO
                  , _rt_conf_fallback_bootstrap_node = [] -- TODO
                  }
