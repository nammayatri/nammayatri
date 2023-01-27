module Environment where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import Lib.GoogleConfig (GoogleCfgUnencrypted)
import Tools.Metrics

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    mockDataPath :: FilePath,
    googleCfg :: Maybe GoogleCfgUnencrypted
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer,
    mockDataPath :: FilePath,
    googleCfg :: Maybe GoogleCfgUnencrypted
  }
  deriving (Generic)

type MockDataFlow m r = HasFlowEnv m r '["mockDataPath" ::: FilePath]

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
