module Utils.ConnectionPoolMetrics
  ( startPgPoolMonitor,
    recordRedisPoolConfig,
  )
where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (tryReadMVar)
import Control.Exception (SomeException, try)
import qualified Data.Foldable as F
import Data.Pool.Internal
import Kernel.Prelude
import qualified Prometheus as P
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Gauges
-- ---------------------------------------------------------------------------

{-# NOINLINE pgPoolSizeGauge #-}
pgPoolSizeGauge :: P.Vector P.Label2 P.Gauge
pgPoolSizeGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "db_pool_size" "Configured maximum connections in PostgreSQL pool"

{-# NOINLINE pgPoolInUseGauge #-}
pgPoolInUseGauge :: P.Vector P.Label2 P.Gauge
pgPoolInUseGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "db_pool_in_use" "PostgreSQL connections currently checked out"

{-# NOINLINE pgPoolIdleGauge #-}
pgPoolIdleGauge :: P.Vector P.Label2 P.Gauge
pgPoolIdleGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "db_pool_idle" "Idle PostgreSQL connections available in pool"

{-# NOINLINE pgPoolUtilizationGauge #-}
pgPoolUtilizationGauge :: P.Vector P.Label2 P.Gauge
pgPoolUtilizationGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "db_pool_utilization_ratio" "PostgreSQL pool utilization (in_use / size)"

{-# NOINLINE redisPoolConfigGauge #-}
redisPoolConfigGauge :: P.Vector P.Label2 P.Gauge
redisPoolConfigGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "redis_pool_max_connections" "Configured maximum connections for Redis pool"

-- ---------------------------------------------------------------------------
-- Pool stats reading
-- ---------------------------------------------------------------------------

readPoolStats :: Pool a -> IO (Int, Int)
readPoolStats pool = do
  let maxPerStripe = poolMaxResources (poolConfig pool)
  stats <- mapM (readStripe maxPerStripe) (F.toList $ localPools pool)
  let totalInUse = sum (map fst stats)
      totalIdle = sum (map snd stats)
  pure (totalInUse, totalIdle)
  where
    readStripe maxRes lp = do
      mbStripe <- tryReadMVar (stripeVar lp)
      case mbStripe of
        Nothing -> pure (0, 0)
        Just stripe ->
          let idle = length (cache stripe)
              inUseCount = maxRes - available stripe - idle
           in pure (max 0 inUseCount, idle)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

startPgPoolMonitor :: Pool a -> Text -> Text -> Int -> IO ThreadId
startPgPoolMonitor pool serviceName poolName maxSize = forkIO $ forever $ do
  result <- try @SomeException $ do
    (currentInUse, currentIdle) <- readPoolStats pool
    let maxD = fromIntegral maxSize :: Double
        inUseD = fromIntegral currentInUse :: Double
        idleD = fromIntegral currentIdle :: Double
        utilization = if maxSize > 0 then inUseD / maxD else 0.0
    P.withLabel pgPoolSizeGauge (serviceName, poolName) (`P.setGauge` maxD)
    P.withLabel pgPoolInUseGauge (serviceName, poolName) (`P.setGauge` inUseD)
    P.withLabel pgPoolIdleGauge (serviceName, poolName) (`P.setGauge` idleD)
    P.withLabel pgPoolUtilizationGauge (serviceName, poolName) (`P.setGauge` utilization)
  case result of
    Left _err -> pure () -- silently continue; monitoring should not crash the service
    Right _ -> pure ()
  threadDelay 10000000 -- 10 seconds

recordRedisPoolConfig :: Text -> Text -> Int -> IO ()
recordRedisPoolConfig serviceName poolName maxConns =
  P.withLabel redisPoolConfigGauge (serviceName, poolName) (`P.setGauge` fromIntegral maxConns)
