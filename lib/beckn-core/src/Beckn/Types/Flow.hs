{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Flow (FlowR, runFlowR) where

import Beckn.Types.Logging
import Beckn.Types.Monitoring.Prometheus.Metrics
import Beckn.Types.Time
import Beckn.Utils.Logging
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R

type FlowR r = ReaderT r L.Flow

runFlowR :: R.FlowRuntime -> r -> FlowR r a -> IO a
runFlowR flowRt r x = I.runFlow flowRt . runReaderT x $ r

instance Log (FlowR r) where
  logOutput = logOutputImplementation
  withLogTag = withLogTagImplementation

instance MonadTime (FlowR r) where
  getCurrentTime = L.runIO getCurrentTime

instance MonadClock (FlowR r) where
  getClockTime = L.runIO getClockTime

instance HasCoreMetrics r => CoreMetrics (FlowR r) where
  addRequestLatency = Metrics.addRequestLatencyFlow
  incrementErrorCounter = Metrics.incrementErrorCounterFlow
