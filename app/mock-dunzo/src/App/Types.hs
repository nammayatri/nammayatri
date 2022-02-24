module App.Types where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Utils.App (getPodName)
import qualified Beckn.Utils.CacheMVar as Cache
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import Tools.Metrics

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    hedisCfg :: Hedis.HedisCfg
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    hedisEnv :: Hedis.HedisEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  isShuttingDown <- mkShutdown
  hedisEnv <- Hedis.connectHedis hedisCfg ("mock-dunzo:" <>)
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance {-# OVERLAPPING #-} CoreMetrics Flow where
  addRequestLatency _ _ _ _ = pure ()
  incrementErrorCounter _ _ = pure ()
  addUrlCallRetries _ _ = pure ()
  addUrlCallRetryFailures _ = pure ()

type RequestId = Text

data TasksCache = TasksCache
  { taskStatusCache :: Cache.CacheMVar API.TaskStatus,
    requestIdCache :: Cache.CacheMVar RequestId
  }

initCache :: IO TasksCache
initCache = do
  taskStatusCache <- Cache.initSimpleCache
  requestIdCache <- Cache.initSimpleCache
  pure TasksCache {..}

instance Cache API.TaskStatus Flow where
  type CacheKey API.TaskStatus = RequestId
  getKey = Hedis.get . ("task-status" <>)
  setKey = Hedis.set . ("task-status" <>)
  delKey = Hedis.del . ("task-status" <>)

instance Cache RequestId Flow where
  type CacheKey RequestId = API.TaskId
  getKey = Hedis.get . ("request-id" <>) . API.getTaskId
  setKey = Hedis.set . ("request-id" <>) . API.getTaskId
  delKey = Hedis.del . ("request-id" <>) . API.getTaskId
