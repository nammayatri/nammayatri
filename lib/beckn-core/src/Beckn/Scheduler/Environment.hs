{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Scheduler.Environment where

import Beckn.Mock.App
import Beckn.Prelude
import Beckn.Scheduler.JobHandler
import Beckn.Scheduler.Metrics (SchedulerMetrics)
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis (HedisCfg, HedisEnv)
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging (LoggerEnv)
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map (Map)

data SchedulerConfig t = SchedulerConfig
  { loggerConfig :: LoggerConfig,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    metricsPort :: Int,
    esqDBCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisPrefix :: Text,
    port :: Int,
    loopIntervalSec :: Int,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    jobType :: Maybe t
  }
  deriving (Generic, FromDhall)

-- this datatype's purpose is to share some resources between the scheduler and the handler function
data SchedulerResources = SchedulerResources
  { esqDBEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    loggerEnv :: LoggerEnv,
    loggerConfig :: LoggerConfig
  }

data SchedulerEnv t = SchedulerEnv
  { esqDBEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    metrics :: SchedulerMetrics,
    handlersMap :: Map t (JobHandler IO t),
    loopIntervalSec :: Int,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    jobType :: Maybe t
  }

newtype SchedulerM t a = SchedulerM {unSchedulerM :: MockM (SchedulerEnv t) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (SchedulerEnv t), MonadIO)
  deriving newtype (C.MonadThrow, C.MonadCatch, C.MonadMask, MonadClock, MonadTime, MonadGuid, Log, Forkable, MonadUnliftIO)

runSchedulerM :: SchedulerEnv t -> SchedulerM t a -> IO a
runSchedulerM env action = runMock env $ unSchedulerM action
