{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Flow (FlowR, runFlowR) where

import Beckn.Utils.Logging
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Prelude (show)

type FlowR r = ReaderT r L.Flow

runFlowR :: R.FlowRuntime -> r -> FlowR r a -> IO a
runFlowR flowRt r x = I.runFlow flowRt . runReaderT x $ r

instance Log (FlowR r) where
  logOutput logLevel message =
    case logLevel of
      DEBUG -> L.logDebug EmtpyTag message
      INFO -> L.logInfo EmtpyTag message
      WARNING -> L.logWarning EmtpyTag message
      ERROR -> L.logError EmtpyTag message
  withLogTag lc flowR =
    ReaderT $ \env ->
      L.withLoggerContext (appendLogContext lc) $
        runReaderT flowR env

data EmtpyTag = EmtpyTag

instance Show EmtpyTag where
  show _ = ""
