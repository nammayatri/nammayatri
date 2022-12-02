module App
  ( runPublicTransportSearchConsumer,
  )
where

import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded)
import Beckn.Types.Flow (runFlowR)
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runHealthCheckServerWithService)
import Beckn.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Environment
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
