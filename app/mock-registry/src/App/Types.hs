module App.Types
  ( Env,
    FlowHandler,
    FlowServer,
    Flow,
    AppCfg (),
    AppEnv (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import Beckn.Storage.Esqueleto.Config
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetricsContainer, registerCoreMetricsContainer)
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import EulerHS.Prelude

data AppCfg = AppCfg
  { port :: Int,
    esqDBCfg :: EsqDBConfig,
    signatureExpiry :: Seconds,
    graceTerminationPeriod :: Seconds,
    loggerConfig :: LoggerConfig,
    autoMigrate :: Bool,
    migrationPath :: Maybe FilePath
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { config :: AppCfg,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  isShuttingDown <- mkShutdown
  coreMetrics <- registerCoreMetricsContainer
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
