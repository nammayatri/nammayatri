module App where

import App.Routes (registryAPI, registryFlow)
import App.Types
import Beckn.Exit (exitDBMigrationFailure)
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerWithHealthCheck)
import Servant (Context (..))

runRegistryService :: (AppCfg -> AppCfg) -> IO ()
runRegistryService configModifier = do
  config <- readDhallConfigDefault "mock-registry" <&> configModifier
  appEnv <- buildAppEnv config
  runServerWithHealthCheck appEnv registryAPI registryFlow middleware identity EmptyContext releaseAppEnv $ \flowRt -> do
    migrateIfNeeded config.migrationPath config.autoMigrate config.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    return flowRt
  where
    middleware = hashBodyForSignature
