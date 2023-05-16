module Lib.Environment where

import AWS.S3
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    esqDBEnv :: EsqDBEnv,
    loggerEnv :: LoggerEnv,
    s3Env :: S3Env Flow,
    esqDBReplicaEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    coreMetrics :: Metrics.CoreMetricsContainer,
    version :: Metrics.DeploymentVersion
  }
