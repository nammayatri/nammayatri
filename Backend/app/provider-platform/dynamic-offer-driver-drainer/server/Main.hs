module Main where

-- import Config.Config as Config
import Config.Env as Env
import qualified Constants as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel)
import qualified DBSync.DBSync as DBSync
import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified "unordered-containers" Data.HashSet as HashSet
import Data.Pool
import Data.Pool.Internal
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam.Postgres
import qualified Euler.Events.Network as NW
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Interpreters as R
import qualified EulerHS.Language as L
import EulerHS.Logger.Types
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import qualified Event.Event as Event
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import Kernel.Storage.Esqueleto.Config (EsqDBConfig)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.Flow
import Kernel.Utils.Dhall hiding (void)
import qualified Kernel.Utils.FlowLogging as L
import qualified Prometheus as P
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Types.DBSync as TDB
import Utils.Utils

main :: IO ()
main = do
  appCfg <- (id :: AppCfg -> AppCfg) <$> readDhallConfigDefault "driver-drainer"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let connString = getConnectionString $ appCfg.esqDBCfg
  connectionPool <- createDbPool appCfg.esqDBCfg
  poolUtilRef <- newIORef 0.0
  void $ startDrainerPoolMonitor connectionPool "driver-drainer" "primary" (appCfg.esqDBCfg.connectionPoolCount) poolUtilRef
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  kafkaProducerTools <- buildKafkaProducerTools' appCfg.kafkaProducerCfg appCfg.secondaryKafkaProducerCfg appCfg.kafkaProperties
  bracket (async NW.runMetricServer) cancel $ \_ -> do
    R.withFlowRuntime
      (Just loggerRt)
      ( \flowRt -> do
          putStrLn @String "Initializing DB and KV Connections..."
          runFlow
            flowRt
            ( prepareConnectionDriver
                ConnectionConfigDriver
                  { esqDBCfg = appCfg.esqDBCfg,
                    esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                    hedisClusterCfg = appCfg.hedisClusterCfg,
                    hedisSecondaryClusterCfg = appCfg.hedisClusterCfg
                  }
                appCfg.kvConfigUpdateFrequency
            )

          dbSyncMetric <- Event.mkDBSyncMetric
          threadPerPodCount <- Env.getThreadPerPodCount
          let environment = Env (T.pack C.kvRedis) dbSyncMetric kafkaProducerTools appCfg.dontEnableForDb appCfg.dontEnableForKafka connectionPool appCfg.esqDBCfg poolUtilRef
          R.runFlow flowRt (runReaderT DBSync.fetchAndSetKvConfigs environment)
          spawnDrainerThread threadPerPodCount flowRt environment
          R.runFlow flowRt (runReaderT DBSync.startDBSync environment)
      )

spawnDrainerThread :: Int -> R.FlowRuntime -> TDB.Env -> IO ()
spawnDrainerThread 0 _ _ = pure ()
spawnDrainerThread count flowRt env = do
  void . forkIO $ R.runFlow flowRt (runReaderT DBSync.startDBSync env)
  spawnDrainerThread (count -1) flowRt env

getConnectionString :: EsqDBConfig -> ByteString
getConnectionString dbConfig =
  TE.encodeUtf8 $
    "host=" <> dbConfig.connectHost
      <> " dbname="
      <> dbConfig.connectDatabase
      <> " user="
      <> dbConfig.connectUser
      <> " password="
      <> dbConfig.connectPassword
      <> " port="
      <> show dbConfig.connectPort

createPoolConfig :: Int -> EsqDBConfig -> PoolConfig Connection
createPoolConfig noOfStripes dbConfig =
  let connectionString = getConnectionString dbConfig
      createConnection = connectPostgreSQL connectionString
   in PoolConfig
        { createResource = createConnection,
          freeResource = close,
          poolCacheTTL = 600,
          poolMaxResources = dbConfig.connectionPoolCount,
          poolNumStripes = Just $ max 1 noOfStripes
        }

createDbPool :: EsqDBConfig -> IO (Pool Connection)
createDbPool dbConfig = do
  noOfStripes <- Env.getThreadPerPodCount
  let poolConfig = createPoolConfig noOfStripes dbConfig
   in newPool poolConfig

-- Connection pool health monitoring (Prometheus gauges)

{-# NOINLINE drainerPoolSizeGauge #-}
drainerPoolSizeGauge :: P.Vector P.Label2 P.Gauge
drainerPoolSizeGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "db_pool_size" "Configured maximum connections in PostgreSQL pool"

{-# NOINLINE drainerPoolInUseGauge #-}
drainerPoolInUseGauge :: P.Vector P.Label2 P.Gauge
drainerPoolInUseGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "db_pool_in_use" "PostgreSQL connections currently checked out"

{-# NOINLINE drainerPoolIdleGauge #-}
drainerPoolIdleGauge :: P.Vector P.Label2 P.Gauge
drainerPoolIdleGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "db_pool_idle" "Idle PostgreSQL connections available in pool"

{-# NOINLINE drainerPoolUtilizationGauge #-}
drainerPoolUtilizationGauge :: P.Vector P.Label2 P.Gauge
drainerPoolUtilizationGauge = unsafePerformIO $
  P.register $
    P.vector ("service", "pool") $
      P.gauge $
        P.Info "db_pool_utilization_ratio" "PostgreSQL pool utilization (in_use / size)"

readDrainerPoolStats :: Pool a -> IO (Int, Int)
readDrainerPoolStats pool = do
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

startDrainerPoolMonitor :: Pool a -> Text -> Text -> Int -> IORef Double -> IO ThreadId
startDrainerPoolMonitor pool serviceName poolName maxSize utilRef = forkIO $ forever $ do
  (currentInUse, currentIdle) <- readDrainerPoolStats pool
  let maxD = fromIntegral maxSize :: Double
      inUseD = fromIntegral currentInUse :: Double
      idleD = fromIntegral currentIdle :: Double
      utilization = if maxSize > 0 then inUseD / maxD else 0.0
  writeIORef utilRef utilization
  P.withLabel drainerPoolSizeGauge (serviceName, poolName) (`P.setGauge` maxD)
  P.withLabel drainerPoolInUseGauge (serviceName, poolName) (`P.setGauge` inUseD)
  P.withLabel drainerPoolIdleGauge (serviceName, poolName) (`P.setGauge` idleD)
  P.withLabel drainerPoolUtilizationGauge (serviceName, poolName) (`P.setGauge` utilization)
  threadDelay 10000000 -- 10 seconds
