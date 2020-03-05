{-# language PatternSynonyms,
             TypeSynonymInstances,
             FlexibleInstances,
             MultiParamTypeClasses,
             InstanceSigs,
             ConstraintKinds,
             FlexibleContexts,
             GeneralizedNewtypeDeriving
#-}

module SHLUG.Seia.Log ( LogEnv(..)
                      , logEnvDefault
                      , WithLogIO
                      , pattern I, pattern D, pattern W
                      , logIO, logIOM, logIOM'
                      , LogIOM
                      , withLogIO
                      ) where

import qualified Data.Text as T
import Data.Text (Text(..))

import Prelude hiding (log)
import Colog ( pattern D, HasLog (..), pattern I, pattern W
             , LogAction(..), Message, WithLog, log
             , richMessageAction, fmtMessage, Severity)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)

import Reflex (PerformEvent(..))

data LogEnv m = MkLogEnv (LogAction (M m) Message)

type M m = ReaderT (LogEnv m) m

instance HasLog (LogEnv m) Message (M m) where
    getLogAction :: LogEnv m -> LogAction (M m) Message
    getLogAction (MkLogEnv x) = x
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction (M m) Message -> LogEnv m -> LogEnv m
    setLogAction newLogAction env = MkLogEnv newLogAction
    {-# INLINE setLogAction #-}

type WithLogIO m = ( MonadReader (LogEnv IO) m
                   , MonadReader (LogEnv IO) (Performable m)
                   )

type LogEnvIO = LogEnv IO

logIO :: LogEnvIO -> Severity -> Text -> IO ()
logIO env sev msg = runReaderT (log sev msg) env

type LogIOM m = Severity -> Text -> m ()

logIOM' :: MonadIO m => LogEnvIO -> LogIOM m
logIOM' env sev msg = liftIO $ logIO env sev msg

logIOM :: (MonadIO m, MonadReader LogEnvIO m) => LogIOM m
logIOM sev msg = do
  env <- ask
  logIOM' env sev msg

logEnvDefault :: LogEnvIO
logEnvDefault = MkLogEnv richMessageAction

withLogIO :: LogEnvIO -> ReaderT LogEnvIO m a -> m a
withLogIO env app = runReaderT app env
