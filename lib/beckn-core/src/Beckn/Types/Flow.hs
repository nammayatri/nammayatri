{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Flow (FlowR) where

import Beckn.Utils.Logging
import EulerHS.Language as L
import EulerHS.Prelude
import Prelude (show)

type FlowR r = ReaderT r L.Flow

instance Log (FlowR r) where
  logOutput logLevel message =
    case logLevel of
      DEBUG -> L.logDebug MockTag message
      INFO -> L.logInfo MockTag message
      WARNING -> L.logWarning MockTag message
      ERROR -> L.logError MockTag message
  withLogContext lc flowR =
    let f = runReaderT flowR
     in ReaderT $ \v -> withModifiedRuntime (updateLogContext lc) $ f v

data MockTag = MockTag

instance Show MockTag where
  show _ = ""