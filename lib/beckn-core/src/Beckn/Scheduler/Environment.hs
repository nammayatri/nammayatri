{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Scheduler.Environment where

import Beckn.Mock.App
import Beckn.Prelude
import Beckn.Scheduler.Types (ExecutionResult, Job)
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis (HedisCfg, HedisEnv)
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging (LoggerEnv)
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Unlift (MonadUnliftIO)

data SchedulerConfig = SchedulerConfig
  { loggerConfig :: LoggerConfig,
    esqDBCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisPrefix :: Text,
    port :: Int,
    loopIntervalSec :: Int,
    expirationTime :: Integer,
    waitBeforeRetry :: Int
  }
  deriving (Generic, FromDhall)

data SchedulerResources = SchedulerResources
  { esqDBEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    loggerEnv :: LoggerEnv
  }

data SchedulerEnv = SchedulerEnv
  { esqDBEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    loggerEnv :: LoggerEnv,
    handlerFunc :: Job -> IO ExecutionResult,
    errorCatchers :: Job -> [C.Handler IO ExecutionResult],
    loopIntervalSec :: Int,
    expirationTime :: Integer,
    waitBeforeRetry :: Int
  }

newtype SchedulerM a = SchedulerM {unSchedulerM :: MockM SchedulerEnv a}
  deriving newtype (Functor, Applicative, Monad, MonadReader SchedulerEnv, MonadIO)
  deriving newtype (MonadThrow, MonadCatch, MonadClock, MonadTime, MonadGuid, Log, Forkable, MonadUnliftIO)

runSchedulerM :: SchedulerEnv -> SchedulerM a -> IO a
runSchedulerM env action = runMock env $ unSchedulerM action
