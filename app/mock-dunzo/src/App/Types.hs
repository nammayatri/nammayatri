module App.Types where

import Beckn.Prelude
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Utils.App (getPodName)
import Beckn.Utils.CacheMVar as Cache
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import Tools.Metrics

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    taskStatusCache :: Cache.CacheMVar API.TaskStatus
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  isShuttingDown <- mkShutdown
  taskStatusCache <- Cache.initSimpleCache
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

instance Cache API.TaskStatus Flow where
  type CacheKey API.TaskStatus = Text
  getKey = Cache.getKey (.taskStatusCache)
  setKey = Cache.setKey (.taskStatusCache)
  delKey = Cache.delKey (.taskStatusCache)

findInCache :: (API.TaskStatus -> Bool) -> Flow [(Text, API.TaskStatus)]
findInCache = Cache.findInCache (.taskStatusCache)
