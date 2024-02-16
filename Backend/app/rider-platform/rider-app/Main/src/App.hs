{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App where

import qualified App.Server as App
import qualified Data.Text as T
import Environment
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import Kernel.Beam.Types (KafkaConn (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Exit
import Kernel.External.AadhaarVerification.Gridline.Config
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import Kernel.Storage.Queries.SystemConfigs
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Common
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth
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
import qualified Storage.CachedQueries.BecknConfig as QBecknConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import System.Environment (lookupEnv)
import "utils" Utils.Common.Events as UE

runRiderApp :: (AppCfg -> AppCfg) -> IO ()
runRiderApp configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "rider-app"
  Metrics.serve (appCfg.metricsPort)
  runRiderApp' appCfg

runRiderApp' :: AppCfg -> IO ()
runRiderApp' appCfg = do
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
      ( ( prepareConnectionRider
            ( ConnectionConfigRider
                { esqDBCfg = appCfg.esqDBCfg,
                  esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
                  hedisClusterCfg = appCfg.hedisClusterCfg
                }
            )
            appCfg.kvConfigUpdateFrequency
        )
          >> L.setOption KafkaConn appEnv.kafkaProducerTools
      )
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo "Setting up for signature auth..."
        kvConfigs <-
          findById "kv_configs" >>= pure . decodeFromText' @Tables
            >>= fromMaybeM (InternalError "Couldn't find kv_configs table for rider app")
        L.setOption KBT.Tables kvConfigs
        allBaps <-
          try QMerchant.loadAllBaps
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allSubscriberIds = map ((.bapId) &&& (.bapUniqueKeyId)) allBaps
        -- Load FRFS BAPs
        frfsBap <-
          try QBecknConfig.findAll
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allFRFSSubIds = map ((.subscriberId) &&& (.uniqueKeyId)) frfsBap
        flowRt' <-
          addAuthManagersToFlowRt
            flowRt
            $ catMaybes
              [ Just (Nothing, prepareAuthManagers flowRt appEnv allSubscriberIds),
                Just (Nothing, prepareAuthManagers flowRt appEnv allFRFSSubIds),
                Just (Just 150000, prepareGridlineHttpManager 150000)
              ]
        logInfo ("Runtime created. Starting server at port " <> show (appCfg.port))
        pure flowRt'
    let timeoutMiddleware = UE.timeoutEvent flowRt appEnv (responseLBS status408 [] "") appCfg.incomingAPIResponseTimeout
    runSettings settings $ timeoutMiddleware (App.run (App.EnvR flowRt' appEnv))
