module Main where

import Config.Env as Env
import qualified Constants as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel)
import qualified DBSync.DBSync as DBSync
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
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import Kernel.Storage.Esqueleto.Config (EsqDBConfig)
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Utils.Dhall hiding (void)
import qualified Kernel.Utils.FlowLogging as L
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import Types.DBSync as TDB
import Utils.Utils

main :: IO ()
main = do
  appCfg <- (id :: AppCfg -> AppCfg) <$> readDhallConfigDefault "rider-drainer"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  connectionPool <- createDbPool appCfg.esqDBCfg
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  kafkaProducerTools <- buildKafkaProducerTools' appCfg.kafkaProducerCfg appCfg.secondaryKafkaProducerCfg appCfg.kafkaProperties
  bracket (async NW.runMetricServer) cancel $ \_ -> do
    R.withFlowRuntime
      (Just loggerRt)
      ( \flowRt -> do
          putStrLn @String "Initializing DB and KV Connections..."
          runFlow
            flowRt
            ( prepareConnectionRider
                ConnectionConfigRider
                  { esqDBCfg = appCfg.esqDBCfg,
                    esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                    hedisClusterCfg = appCfg.hedisClusterCfg,
                    hedisSecondaryClusterCfg = appCfg.hedisClusterCfg
                  }
                appCfg.kvConfigUpdateFrequency
            )

          dbSyncMetric <- Event.mkDBSyncMetric
          let pgDropCols = appCfg.dropColumnsForDb <> appCfg.dropColumnsForBoth
              chDropCols = appCfg.dropColumnsForCh <> appCfg.dropColumnsForBoth
              pgDropTables = appCfg.dropTablesForDb <> appCfg.dropTablesForBoth
              chDropTables = appCfg.dropTablesForCh <> appCfg.dropTablesForBoth
              -- A dropColumns entry must be exactly "table.column" (single dot, snake_case, no db prefix).
              -- Anything else silently no-ops at match time, so warn loudly at startup instead of wedging the
              -- stream later when the still-emitted dropped column hits PostgreSQL.
              malformedDropCols =
                filter (\c -> let parts = T.splitOn "." c in length parts /= 2 || any T.null parts) $
                  appCfg.dropColumnsForDb <> appCfg.dropColumnsForCh <> appCfg.dropColumnsForBoth
          normalThreadCount <- Env.getThreadPerPodCount
          criticalThreadCount <- Env.getCriticalThreadPerPodCount
          let environment = Env (T.pack C.kvRedis) dbSyncMetric kafkaProducerTools appCfg.dontEnableForDb appCfg.dontEnableForKafka connectionPool (appCfg.esqDBCfg) pgDropCols chDropCols pgDropTables chDropTables
          R.runFlow flowRt $
            L.logInfo ("SchemaDrop" :: T.Text) $
              "[DropConfig] PG tables=" <> T.pack (show pgDropTables)
                <> " PG cols="
                <> T.pack (show pgDropCols)
                <> " CH tables="
                <> T.pack (show chDropTables)
                <> " CH cols="
                <> T.pack (show chDropCols)
          unless (null malformedDropCols) $
            R.runFlow flowRt $
              L.logWarning ("SchemaDrop" :: T.Text) $
                "[DropConfig] Ignoring malformed dropColumns entries (expected exactly 'table.column', snake_case, no db prefix): "
                  <> T.pack (show malformedDropCols)
          R.runFlow flowRt (runReaderT DBSync.fetchAndSetKvConfigs environment)
          -- one thread per stream by default; set either env count to 0 to stop draining that stream
          spawnDrainerThread criticalThreadCount True flowRt environment
          spawnDrainerThread normalThreadCount False flowRt environment
          forever $ threadDelay 60000000
      )

spawnDrainerThread :: Int -> Bool -> R.FlowRuntime -> TDB.Env -> IO ()
spawnDrainerThread count isCritical flowRt env
  | count <= 0 = pure ()
  | otherwise = do
    void . forkIO $ R.runFlow flowRt (runReaderT (DBSync.startDBSync isCritical) env)
    spawnDrainerThread (count -1) isCritical flowRt env

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
