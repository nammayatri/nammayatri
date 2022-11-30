{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fixtures.TestRedis where

import Beckn.Storage.Hedis.AppPrefixes
import Beckn.Storage.Hedis.Config
import Beckn.Tools.Metrics.CoreMetrics.Types
import Beckn.Types.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

data MockEnv = MockEnv
  { hedisEnv :: HedisEnv,
    coreMetrics :: CoreMetricsContainer
  }

type WithRedisMonad = ReaderT MockEnv IO

instance Log (ReaderT MockEnv IO) where
  logOutput _logLevel _msg = pure ()
  withLogTag _ a = a

instance MonadTime WithRedisMonad where
  getCurrentTime = lift getCurrentTime

instance CoreMetrics WithRedisMonad

instance L.MonadFlow IO

instance Forkable WithRedisMonad

instance MonadGuid WithRedisMonad

instance MonadClock WithRedisMonad

runWithMockHedis :: WithRedisMonad a -> IO a
runWithMockHedis f = do
  env <- connectHedis defaultHedisCfg becknTransportPrefix
  metrics <- registerCoreMetricsContainer
  runReaderT f (MockEnv env metrics)
