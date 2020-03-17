{-# language ForeignFunctionInterface, JavaScriptFFI #-}
{-# language PatternSynonyms, LambdaCase #-}

module SHLUG.Seia.Rt ( isNodeJS
                     , js_rt
                     , consoleLog
                     , bs_to_u8a
                     , u8a_to_bs
                     , jsval_to_bs
                     , u8a_to_jsval
                     , rtConf
                     , RtConf(..)
                     , sign, verify, seed2sk, sign_verify_benchmark
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
                                   , js0, js1, js2
                                   )

import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe(unsafePerformIO)

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
import Data.Maybe (fromMaybe, mapMaybe)

import Control.Lens ((^.))

import Data.Text ( Text(..) )
import qualified Data.Text as T
import Data.Word ( Word16 )

import Text.Printf
import Data.Time

import Colog ( pattern D, pattern I, pattern W, pattern E
             , Severity
             )

-- major for protocol compatiable check
mainVersion :: (Int, Int)
mainVersion = (2, 0)

foreign import javascript unsafe "$r = _rt;"
  _rt :: IO JSVal

foreign import javascript unsafe "$r = console;"
  _console :: IO JSVal

foreign import javascript interruptible
  "_rt.store.get($1, function (err, res) { $c(err, res); });"
  store_get_a :: JSString -> IO (JSVal, Uint8Array) -- IO (TypeOfErr, Uint8Array)

foreign import javascript interruptible
  "_rt.store.set($1, $2, $c);"
  store_set_a :: JSString -> Uint8Array -> IO JSVal -- IO (TypeOfErr)

foreign import javascript interruptible
  "_rt.store.exist($1, $c);"
  store_exist :: JSString -> IO JSVal -- IO Bool

foreign import javascript interruptible
  "_rt.store.remove($1, $c);"
  store_remove :: JSString -> IO JSVal -- IO TypeOfErr

foreign import javascript unsafe "new Uint8Array(new ArrayBuffer(0))"
  js_empty_u8a :: IO Uint8Array

foreign import javascript unsafe "_rt.is_nodejs()"
  js_rt_is_nodejs :: IO Bool

foreign import javascript unsafe "_rt.VERSION"
  js_rt_version :: IO Int

foreign import javascript unsafe "_rt.preloader_url"
  js_rt_preloader_url :: IO JSString

foreign import javascript unsafe
  "if (typeof(_rt.sid) == 'number') { $r = _rt.sid; } else { $r = -1; }"
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

js_rt :: JSM JSVal
js_rt = liftIO _rt

consoleLog :: ToJSVal a => a -> JSM ()
consoleLog a = do
  -- NOTE: jsg will get object from 'window' object
  console <- liftIO _console
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
  res <- liftIO js_rt_is_nodejs
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
            , _rt_main_version :: (Int, Int)
            , _rt_seq :: Int -- current last seq
            , _rt_seq_curr :: Int -- current seq actually used
            -- from windw._rt.conf
            , _rt_conf_turn_server :: [Text]
            , _rt_conf_fallback_bootstrap_node :: [NID]
            , _rt_conf_log_level :: [Severity]
            , _rt_conf_service :: [Text]
            , _rt_mqtt_server :: Text
            } deriving (Eq, Show)

rtConf :: JSM RtConf
rtConf = do
  is_nodejs <- liftIO isNodeJS
  sid' <- liftIO $ js_rt_sid
  ver <- liftIO js_rt_version
  url <- liftIO js_rt_preloader_url

  ts' <- js_rt ^. js "conf" ^. js "turn_server"
  bn' <- js_rt ^. js "conf" ^. js "bootstrap_node"

  ts <- fromMaybe [] <$> fromJSVal ts'
  bn1 <- fromMaybe [] <$> fromJSVal bn'

  let bn = map read bn1

  mqtt_server' <- js_rt ^. js "conf" ^. js "mqtt_server"
  mqtt_server <- fromMaybe T.empty <$> fromJSVal mqtt_server'

  lv <- fromMaybe [] <$>
        (js_rt ^. js "conf" ^.js "log_level" >>= fromJSVal) :: JSM [String]
  let lv' = mapMaybe (\case "I" -> Just I
                            "D" -> Just D
                            "E" -> Just E
                            "W" -> Just W
                            _   -> Nothing) lv

  sv <- fromMaybe [] <$>
        (js_rt ^. js "conf" ^. js "service" >>= fromJSVal)

  seq <- fromMaybe 1 <$> (js_rt ^. js "SEQ" >>= fromJSVal)
  seq_curr <- fromMaybe 1 <$> (js_rt ^. js "SEQ_CURR" >>= fromJSVal)

  return $ RtConf { _rt_is_nodejs = is_nodejs
                  , _rt_sid = if sid' < 0 then Nothing else Just (toEnum sid')
                  , _rt_preloader_url = fromJSString url
                  , _rt_main_version = mainVersion
                  , _rt_seq = seq
                  , _rt_seq_curr= seq_curr
                  --
                  , _rt_conf_turn_server = ts
                  , _rt_conf_fallback_bootstrap_node = bn
                  , _rt_conf_log_level = lv'
                  , _rt_conf_service = sv
                  , _rt_mqtt_server = mqtt_server
                  }


foreign import javascript unsafe "_rt.rust_crypto_ed25519.sign($1,$2)"
  js_rust_crypto_sign :: Uint8Array -> Uint8Array -> IO Uint8Array

foreign import javascript unsafe "_rt.rust_crypto_ed25519.verify($1,$2,$3)"
  js_rust_crypto_verify :: Uint8Array -> Uint8Array -> Uint8Array -> IO Bool

foreign import javascript unsafe "_rt.rust_crypto_ed25519.keypair($1)"
  js_rust_crypto_keypair :: Uint8Array -> IO Uint8Array

seed2sk :: ByteString -> ByteString
seed2sk seed = unsafePerformIO $ do
  seed' <- bs_to_u8a seed
  pair <- js_rust_crypto_keypair seed' -- first 64B is sk, last 32B is pk
  sk' <- ghcjsPure $ subarray 0 64 pair
  u8a_to_bs sk'

sign :: ByteString -> ByteString -> ByteString
sign = sign4

sign4 sk dat = unsafePerformIO $ do
  sk' <- bs_to_u8a sk
  dat' <- bs_to_u8a dat

  res <- liftIO $ js_rust_crypto_sign dat' sk'
  u8a_to_bs res


verify :: ByteString -> ByteString -> ByteString -> Bool
verify = verify4

verify4 pk sig dat = unsafePerformIO $ do
  dat' <- bs_to_u8a dat
  sig' <- bs_to_u8a sig
  pk' <- bs_to_u8a pk
  liftIO $ js_rust_crypto_verify dat' pk' sig'


sign_verify_benchmark :: (ByteString, ByteString) -> Int -> IO ()
sign_verify_benchmark (sk, pk) n = do
  let dat n = BS.pack $ take 16 $ drop n $ cycle [0..15]
  t0 <- getCurrentTime
  printf "sign_verify_benchmark start...\n"
  let res = map (\x -> let y = dat x in verify pk (sign sk y) y) [1..n]
  let res1 = all id res
  printf "sign_verify_benchmark result: %s\n" (show $ all id res)
  t1 <- getCurrentTime
  printf "sign_verify_benchmark(%d) time %s\n"
         n  (show $ diffUTCTime t1 t0)
