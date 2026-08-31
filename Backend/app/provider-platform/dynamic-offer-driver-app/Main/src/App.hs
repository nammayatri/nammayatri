{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App where

import AWS.S3
import qualified App.Server as App
import qualified Client.Main as CM
import qualified Control.Concurrent.MVar as MV
import qualified Data.Bool as B
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Proxy as DP
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Database.ClickHouseDriver.HTTP as CHTTP
import Environment
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.KVConnector.Metrics as KVCM
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types (ConnectionConfigDriver (..))
import Kernel.Beam.Types (KafkaConn (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Exit
import Kernel.External.AadhaarVerification.Gridline.Config
import Kernel.External.Insurance.Interface (prepareIffcoTokioHttpManager)
import Kernel.External.SharedLogic.HyperVerge.Functions (prepareHyperVergeHttpManager)
import Kernel.External.Tokenize (prepareJourneyMonitoringHttpManager)
import Kernel.External.Verification.Ekatra.Types (prepareEkatraHttpManager)
import Kernel.External.Verification.Interface (prepareMorthHttpManager)
import Kernel.External.Verification.Interface.Idfy
import Kernel.External.Verification.InternalScripts.FaceVerification (prepareInternalScriptsHttpManager)
import Kernel.External.Verification.InternalScripts.InternalOCR (prepareInternalOCRHttpManager)
import Kernel.External.Verification.SafetyPortal.Config (prepareSafetyPortalHttpManager)
import qualified Kernel.Storage.Beam.MerchantOperatingCity as Beam
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import Kernel.Storage.Queries.SystemConfigs
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Beckn.City (initCityMaps)
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Common
import Kernel.Utils.Dhall hiding (maybe)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth (addAuthManagersToFlowRt, prepareAuthManagers)
import Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTLS
import Network.HTTP.Types (status408)
import Network.Wai
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as Storage
import System.Environment (lookupEnv)
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)
import "utils" Utils.Common.Events as UE

instance HasSchemaName Beam.MerchantOperatingCityT where
  schemaName _ = T.pack currentSchemaName

createCAC :: AppCfg -> IO ()
createCAC appCfg = do
  when appCfg.cacConfig.enableCac $ do
    cacStatus <- CM.initCACClient appCfg.cacConfig.host (fromIntegral appCfg.cacConfig.interval) appCfg.cacTenants appCfg.cacConfig.enablePolling
    case cacStatus of
      0 -> CM.startCACPolling appCfg.cacTenants
      _ -> do
        -- logError "CAC client failed to start"
        threadDelay 1000000
        B.bool (pure ()) (createCAC appCfg) appCfg.cacConfig.retryConnection
  when appCfg.superPositionConfig.enableSuperPosition $ do
    superPositionStatus <- CM.initSuperPositionClient appCfg.superPositionConfig.host (fromIntegral appCfg.superPositionConfig.interval) appCfg.superPositionConfig.tenants appCfg.superPositionConfig.enablePolling
    case superPositionStatus of
      0 -> CM.runSuperPositionPolling appCfg.superPositionConfig.tenants
      _ -> do
        -- logError "CAC super position client failed to start"
        threadDelay 1000000
        B.bool (pure ()) (createCAC appCfg) appCfg.cacConfig.retryConnection

runDynamicOfferDriverApp :: (AppCfg -> AppCfg) -> IO ()
runDynamicOfferDriverApp configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "dynamic-offer-driver-app"
  Metrics.serve (appCfg.metricsPort)
  runDynamicOfferDriverApp' appCfg

runDynamicOfferDriverApp' :: AppCfg -> IO ()
runDynamicOfferDriverApp' appCfg = do
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
          & setPort (appCfg.port)
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlow
      flowRt
      ( ( prepareConnectionDriver
            ConnectionConfigDriver
              { esqDBCfg = appCfg.esqDBCfg,
                esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                hedisClusterCfg = appCfg.hedisClusterCfg,
                hedisSecondaryClusterCfg = appCfg.hedisSecondaryClusterCfg
              }
            appCfg.kvConfigUpdateFrequency
        )
          >> L.setOption KafkaConn appEnv.kafkaProducerTools
          >> L.setOption KVCM.KVMetricCfg appEnv.coreMetrics.kvRedisMetricsContainer
      )

    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo "Setting up for signature auth..."
        kvConfigs <-
          findById "kv_configs" >>= pure . decodeFromText' @Tables
            >>= fromMaybeM (InternalError "Couldn't find kv_configs table for driver app")
        L.setOption KBT.Tables kvConfigs
        chDropTables <- liftIO $ readEnvList "DRIVER_APP_CH_DROP_TABLES"
        chDropColumns <- liftIO $ readEnvList "DRIVER_APP_CH_DROP_COLUMNS"
        -- Background drop of unused ClickHouse tables/columns; off the startup path.
        fork "Clickhouse schema drops" $ runClickhouseSchemaDrops chDropTables chDropColumns
        _ <- liftIO $ createCAC appCfg
        initCityMaps
        allProviders <-
          try Storage.loadAllProviders
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allSubscriberIds = map ((.subscriberId.getShortId) &&& (.uniqueKeyId)) allProviders
        flowRt' <-
          addAuthManagersToFlowRt
            flowRt
            $ catMaybes
              [ Just (Nothing, prepareAuthManagers flowRt appEnv allSubscriberIds),
                (Nothing,) <$> mkS3MbManager flowRt appEnv appCfg.s3Config,
                Just (Just 20000, prepareIdfyHttpManager 20000),
                Just (Just 10000, prepareInternalScriptsHttpManager 10000),
                Just (Just 10000, prepareSafetyPortalHttpManager 10000),
                Just (Just 150000, prepareGridlineHttpManager 150000),
                Just (Just 10000, prepareHyperVergeHttpManager 10000),
                Just (Just 10000, prepareJourneyMonitoringHttpManager 10000),
                Just (Just 40000, prepareIffcoTokioHttpManager 40000),
                Just (Just 15000, prepareMorthHttpManager 15000),
                Just (Just 150000, prepareEkatraHttpManager 150000),
                Just (Just 10000, prepareInternalOCRHttpManager 10000)
              ]

        logInfo ("Runtime created. Starting server at port " <> show (appCfg.port))
        pure flowRt'
    let timeoutMiddleware = UE.timeoutEvent flowRt appEnv (responseLBS status408 [] "") appCfg.incomingAPIResponseTimeout
    proxyManager <- HttpTLS.newTlsManager
    runSettings settings $ timeoutMiddleware (App.run proxyManager (App.EnvR flowRt' appEnv))

convertToHashMap :: Map.Map String Http.ManagerSettings -> HashMap.HashMap Text Http.ManagerSettings
convertToHashMap = HashMap.fromList . map convert . Map.toList
  where
    convert (k, v) = (toText' k, v)
    toText' = T.pack

readEnvList :: String -> IO [Text]
readEnvList var = do
  mbVal <- lookupEnv var
  pure $ maybe [] (filter (not . T.null) . map T.strip . T.splitOn "," . T.pack) mbVal

-- | Drop DRIVER_APP_CH_DROP_TABLES / _COLUMNS (IF EXISTS) in the background;
--   logs and skips failures. Optional per-entry cluster via '@':
--   [cluster@][db.]table  and  [cluster@][db.]table.column.
runClickhouseSchemaDrops ::
  (MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  [Text] ->
  [Text] ->
  m ()
runClickhouseSchemaDrops [] [] = pure ()
runClickhouseSchemaDrops dropTables dropColumns = do
  chEnv <- CH.getClickhouseEnv (DP.Proxy @CH.APP_SERVICE_CLICKHOUSE)
  forM_ dropTables $ \e ->
    case splitCluster e of
      Just (onCluster, table) | isValidName table -> runDrop chEnv $ "DROP TABLE IF EXISTS " <> table <> onCluster
      _ -> logError $ "[CH SchemaDrop] Skipping malformed/unsafe table entry ([cluster@][db.]table): " <> e
  forM_ dropColumns $ \e ->
    case splitCluster e of
      Just (onCluster, rest) ->
        case T.breakOnEnd "." rest of
          (tbl, col)
            | not (T.null tbl) && not (T.null col) && isValidName rest ->
              runDrop chEnv $ "ALTER TABLE " <> T.dropEnd 1 tbl <> onCluster <> " DROP COLUMN IF EXISTS " <> col
          _ -> logMalformedCol e
      Nothing -> logMalformedCol e
  where
    -- Split an optional leading "cluster@" prefix; returns (ON CLUSTER clause, rest).
    -- No '@' => no cluster. Dots keep their db.table.column meaning in the rest.
    splitCluster e = case T.splitOn "@" e of
      [rest] -> Just ("", rest)
      [c, rest] | isClusterName c -> Just (" ON CLUSTER `" <> c <> "`", rest)
      _ -> Nothing
    isClusterName c = not (T.null c) && T.all (\ch -> isIdent ch || ch == '-') c
    isValidName t =
      not (T.null t)
        && not (T.isPrefixOf "." t)
        && not (T.isSuffixOf "." t)
        && T.all (\c -> c == '.' || isIdent c) t
    isIdent c = c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c
    logMalformedCol e = logError $ "[CH SchemaDrop] Skipping malformed/unsafe column entry ([cluster@][db.]table.column): " <> e
    runDrop chEnv stmt = do
      logInfo $ "[CH SchemaDrop] Executing: " <> stmt
      res <- L.runIO $
        try $ do
          connData <- MV.readMVar chEnv.connectionData
          CHTTP.exec (T.unpack stmt) connData.connection
      case res of
        Left (e :: SomeException) -> logError $ "[CH SchemaDrop] FAILED: " <> stmt <> " => " <> show e
        Right (Left errBytes) -> logError $ "[CH SchemaDrop] FAILED: " <> stmt <> " => " <> cs errBytes
        Right (Right _) -> logInfo $ "[CH SchemaDrop] OK: " <> stmt
