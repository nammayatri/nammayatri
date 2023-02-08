{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Scheduler.Environment where

import qualified Control.Monad.Catch as C
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Kernel.Mock.App
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, disconnectHedis)
import Kernel.Types.Common
import Kernel.Utils.App (Shutdown)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging (LoggerEnv, releaseLoggerEnv)
import Lib.Scheduler.Metrics (SchedulerMetrics)

data SchedulerConfig = SchedulerConfig
  { loggerConfig :: LoggerConfig,
    metricsPort :: Int,
    esqDBCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisPrefix :: Text,
    port :: Int,
    loopIntervalSec :: Seconds,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    tasksPerIteration :: Int,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

data SchedulerEnv = SchedulerEnv
  { esqDBEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    metrics :: SchedulerMetrics,
    loopIntervalSec :: Seconds,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    tasksPerIteration :: Int,
    graceTerminationPeriod :: Seconds,
    port :: Int,
    isShuttingDown :: Shutdown
  }
  deriving (Generic)

releaseSchedulerEnv :: SchedulerEnv -> IO ()
releaseSchedulerEnv SchedulerEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv

newtype SchedulerM a = SchedulerM {unSchedulerM :: MockM SchedulerEnv a}
  deriving newtype (Functor, Applicative, Monad, MonadReader SchedulerEnv, MonadIO)
  deriving newtype (C.MonadThrow, C.MonadCatch, C.MonadMask, MonadClock, MonadTime, MonadGuid, Log, Forkable, MonadUnliftIO)

runSchedulerM :: SchedulerEnv -> SchedulerM a -> IO a
runSchedulerM env action = runMock env $ unSchedulerM action
