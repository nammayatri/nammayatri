{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App where

import AWS.S3
import qualified App.Server as App
import qualified Data.Text as T
import Environment
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Idfy.Auth
import Kernel.Exit
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth (addAuthManagersToFlowRt)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import qualified Storage.CachedQueries.Merchant as Storage
import System.Environment (lookupEnv)
import Tools.SignatureAuth

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
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "

        logInfo "Setting up for signature auth..."
        allProviders <-
          try (Storage.loadAllProviders (Proxy @Flow))
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allSubscriberIds = map ((.subscriberId.getShortId) &&& (.uniqueKeyId)) allProviders
        flowRt' <-
          addAuthManagersToFlowRt
            flowRt
            $ catMaybes
              [ Just (Nothing, prepareAuthManagersWithRegistryUrl flowRt appEnv allSubscriberIds),
                (Nothing,) <$> mkS3MbManager flowRt appEnv appCfg.s3Config,
                Just (Just 20000, prepareIdfyHttpManager 20000)
              ]

        logInfo ("Runtime created. Starting server at port " <> show (appCfg.port))
        pure flowRt'
    runSettings settings $ App.run (App.EnvR flowRt' appEnv)
