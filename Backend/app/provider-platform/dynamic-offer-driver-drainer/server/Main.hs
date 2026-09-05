module Main where

-- import Config.Config as Config
import Config.Env as Env
import qualified Constants as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel)
import qualified DBSync.DBSync as DBSync
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.HashSet as HS
import qualified "unordered-containers" Data.HashSet as HashSet
import Data.Pool
import Data.Pool.Internal
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam.Postgres
import Database.PostgreSQL.Simple (execute_)
import Database.PostgreSQL.Simple.Types (Query (..))
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
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import Types.DBSync as TDB
import Utils.Utils

main :: IO ()
main = do
  appCfg <- (id :: AppCfg -> AppCfg) <$> readDhallConfigDefault "driver-drainer"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let connString = getConnectionString $ appCfg.esqDBCfg
  connectionPool <- createDbPool appCfg.esqDBCfg
  dropTables <- readEnvList "DRIVER_DRAINER_DROP_TABLES"
  dropColumns <- readEnvList "DRIVER_DRAINER_DROP_COLUMNS"
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
          normalThreadCount <- Env.getThreadPerPodCount
          criticalThreadCount <- Env.getCriticalThreadPerPodCount
          let environment = Env (T.pack C.kvRedis) dbSyncMetric kafkaProducerTools appCfg.dontEnableForDb appCfg.dontEnableForKafka connectionPool appCfg.esqDBCfg
          R.runFlow flowRt (runReaderT DBSync.fetchAndSetKvConfigs environment)
          -- one thread per stream by default; set either env count to 0 to stop draining that stream
          spawnDrainerThread criticalThreadCount True flowRt environment
          spawnDrainerThread normalThreadCount False flowRt environment
          -- Background drop of unused tables/columns; never blocks draining.
          void $ forkIO $ runSchemaDrops flowRt connString appCfg.esqDBCfg.connectSchemaName dropTables dropColumns
          forever $ threadDelay 60000000
      )

spawnDrainerThread :: Int -> Bool -> R.FlowRuntime -> TDB.Env -> IO ()
spawnDrainerThread count isCritical flowRt env
  | count <= 0 = pure ()
  | otherwise = do
    void . forkIO $ R.runFlow flowRt (runReaderT (DBSync.startDBSync isCritical) env)
    spawnDrainerThread (count -1) isCritical flowRt env

readEnvList :: String -> IO [Text]
readEnvList var = do
  mbVal <- lookupEnv var
  pure $ case mbVal of
    Nothing -> []
    Just s -> filter (not . T.null) . map T.strip . T.splitOn "," $ T.pack s

-- | Drop DRIVER_DRAINER_DROP_TABLES / _COLUMNS (IF EXISTS) on a dedicated
--   connection in the background; logs and skips failures.
runSchemaDrops :: R.FlowRuntime -> ByteString -> Text -> [Text] -> [Text] -> IO ()
runSchemaDrops flowRt connString schemaName dropTables dropColumns =
  when (not (null dropTables) || not (null dropColumns)) $ do
    res <-
      try $
        bracket (connectPostgreSQL connString) close $ \conn -> do
          forM_ dropTables $ \tbl ->
            if isValidName tbl
              then runDrop conn $ "DROP TABLE IF EXISTS " <> qualify tbl
              else R.runFlow flowRt $ L.logError ("SchemaDrop" :: Text) $ "[SchemaDrop] Skipping unsafe table name: " <> tbl
          forM_ dropColumns $ \tc ->
            case T.breakOnEnd "." tc of
              (tbl, col)
                | not (T.null tbl) && not (T.null col) && isValidName tc ->
                  runDrop conn $ "ALTER TABLE " <> qualify (T.dropEnd 1 tbl) <> " DROP COLUMN IF EXISTS " <> col
              _ -> R.runFlow flowRt $ L.logError ("SchemaDrop" :: Text) $ "[SchemaDrop] Skipping malformed or unsafe column entry (expected table.column): " <> tc
    case (res :: Either SomeException ()) of
      Left e -> R.runFlow flowRt $ L.logError ("SchemaDrop" :: Text) $ "[SchemaDrop] Connection failed, drops skipped: " <> T.pack (show e)
      Right () -> pure ()
  where
    qualify t = if "." `T.isInfixOf` t then t else schemaName <> "." <> t
    isValidName t =
      not (T.null t)
        && not (T.isPrefixOf "." t)
        && not (T.isSuffixOf "." t)
        && T.all (\c -> c == '_' || c == '.' || isAsciiLower c || isAsciiUpper c || isDigit c) t
    runDrop conn stmt = do
      R.runFlow flowRt $ L.logInfo ("SchemaDrop" :: Text) $ "[SchemaDrop] Executing: " <> stmt
      res <- (try $ execute_ conn (Query $ TE.encodeUtf8 stmt)) :: IO (Either SomeException Int64)
      case res of
        Left e -> R.runFlow flowRt $ L.logError ("SchemaDrop" :: Text) $ "[SchemaDrop] FAILED: " <> stmt <> " => " <> T.pack (show e)
        Right _ -> R.runFlow flowRt $ L.logInfo ("SchemaDrop" :: Text) $ "[SchemaDrop] OK: " <> stmt

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
