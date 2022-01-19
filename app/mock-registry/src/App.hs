module App where

import App.Routes (registryAPI, registryFlow)
import App.Types
import Beckn.Exit (exitDBMigrationFailure)
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerService)
import Servant (Context (..))

runRegistryService :: (AppCfg -> AppCfg) -> IO ()
runRegistryService configModifier = do
  appEnv <- readDhallConfigDefault "mock-registry" <&> configModifier >>= buildAppEnv
  runServerService appEnv registryAPI registryFlow middleware identity EmptyContext releaseAppEnv $ \flowRt -> do
    migrateIfNeeded (appEnv.config.migrationPath) (appEnv.config.esqDBCfg) (appEnv.config.autoMigrate)
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    return flowRt
  where
    middleware = hashBodyForSignature
