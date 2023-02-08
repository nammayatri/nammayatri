module App
  ( runPublicTransportSearchConsumer,
  )
where

import Environment
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runHealthCheckServerWithService)
import Kernel.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Servant
import qualified Service.Runner as Runner

runPublicTransportSearchConsumer :: (AppCfg -> AppCfg) -> IO ()
runPublicTransportSearchConsumer configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "public-transport-search-consumer"
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "

  runHealthCheckServerWithService appEnv identity identity EmptyContext (runService appEnv) releaseAppEnv $ \flowRt -> do
    migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    modFlowRtWithAuthManagers flowRt appEnv [(appCfg.bapId, appCfg.authEntity.uniqueKeyId)]
  where
    runService appEnv flowRt =
      runFlowR flowRt appEnv Runner.run
