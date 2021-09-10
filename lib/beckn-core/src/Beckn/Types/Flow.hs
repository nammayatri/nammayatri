{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Flow (FlowR, runFlowR) where

import Beckn.Types.Forkable
import Beckn.Types.Logging
import Beckn.Types.MonadGuid
import Beckn.Types.Monitoring.Prometheus.Metrics
import Beckn.Types.Time
import qualified Beckn.Utils.IOLogging as IOLogging
import Beckn.Utils.Logging
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Prometheus (MonadMonitor (..))

runFlowR :: R.FlowRuntime -> r -> FlowR r a -> IO a
runFlowR flowRt r (FlowR x) = I.runFlow flowRt . runReaderT x $ r

newtype FlowR r a = FlowR {unFlowR :: ReaderT r L.Flow a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader r,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

instance L.MonadFlow (FlowR r) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url cl = FlowR $ L.callServantAPI mbMgrSel url cl
  {-# INLINEABLE callHTTPWithCert #-}
  callHTTPWithCert url cert = FlowR $ L.callHTTPWithCert url cert
  {-# INLINEABLE evalLogger' #-}
  evalLogger' logAct = FlowR $ L.evalLogger' logAct
  {-# INLINEABLE runIO' #-}
  runIO' descr ioAct = FlowR $ L.runIO' descr ioAct
  {-# INLINEABLE getOption #-}
  getOption k = FlowR $ L.getOption k
  {-# INLINEABLE setOption #-}
  setOption k v = FlowR $ L.setOption k v
  {-# INLINEABLE delOption #-}
  delOption k = FlowR $ L.delOption k
  {-# INLINEABLE generateGUID #-}
  generateGUID = FlowR L.generateGUID
  {-# INLINEABLE runSysCmd #-}
  runSysCmd cmd = FlowR $ L.runSysCmd cmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection cfg = FlowR $ L.initSqlDBConnection cfg
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection conn = FlowR $ L.deinitSqlDBConnection conn
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection cfg = FlowR $ L.getSqlDBConnection cfg
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection cfg = FlowR $ L.initKVDBConnection cfg
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection conn = FlowR $ L.deinitKVDBConnection conn
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection cfg = FlowR $ L.getKVDBConnection cfg
  {-# INLINEABLE runDB #-}
  runDB conn dbAct = FlowR $ L.runDB conn dbAct
  {-# INLINEABLE runTransaction #-}
  runTransaction conn dbAct = FlowR $ L.runTransaction conn dbAct
  {-# INLINEABLE await #-}
  await mbMcs awaitable = FlowR $ L.await mbMcs awaitable
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow flow = FlowR $ L.runSafeFlow flow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName act = FlowR $ L.runKVDB cName act
  {-# INLINEABLE runPubSub #-}
  runPubSub act = FlowR $ L.runPubSub act
  {-# INLINEABLE publish #-}
  publish channel payload = FlowR $ L.publish channel payload
  {-# INLINEABLE subscribe #-}
  subscribe channels cb = FlowR $ L.subscribe channels cb
  {-# INLINEABLE psubscribe #-}
  psubscribe channels cb = FlowR $ L.psubscribe channels cb
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f flow = FlowR $ L.withModifiedRuntime f flow

instance MonadIO (FlowR r) where
  liftIO = FlowR . L.runIO

instance {-# OVERLAPPABLE #-} IOLogging.HasLog r => Log (FlowR r) where
  logOutput = IOLogging.logOutputImplementation
  withLogTag lc (FlowR flowR) = FlowR $ IOLogging.withLogTagImplementation lc flowR

instance MonadTime (FlowR r) where
  getCurrentTime = liftIO getCurrentTime

instance MonadClock (FlowR r) where
  getClockTime = liftIO getClockTime

instance HasCoreMetrics r => CoreMetrics (FlowR r) where
  addRequestLatency = Metrics.addRequestLatency
  incrementErrorCounter = Metrics.incrementErrorCounter
  addUrlCallRetries = Metrics.addUrlCallRetries
  addUrlCallRetryFailures = Metrics.addUrlCallFailures

instance MonadMonitor (FlowR r) where
  doIO = liftIO

instance MonadGuid (FlowR r) where
  generateGUIDText = FlowR L.generateGUID

instance (Log (FlowR r), HasCoreMetrics r) => Forkable (FlowR r) where
  fork desc f = do
    FlowR $ ReaderT $ L.forkFlow desc . runReaderT (unFlowR $ handleExc f)
    where
      handleExc = try >=> (`whenLeft` err)
      err (e :: SomeException) = do
        logError $ "Thread " <> show desc <> " died with error: " <> makeLogSomeException e
        incrementErrorCounter "FORKED_THREAD_ERROR" e
