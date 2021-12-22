module App
  ( runService,
  )
where

import API.Handler
import API.Types
import App.Types
import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded)
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerService)
import Beckn.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Servant (Context (..))
import Tools.Auth

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "parking-bap" <&> configModifier
  appEnv <- buildAppEnv appCfg
  runServerService appEnv (Proxy @API) handler middleware identity context releaseAppEnv \flowRt -> do
    try (prepareRedisConnections $ appCfg.redisCfg)
      >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
    migrateIfNeeded (appCfg.migrationPath) (appCfg.esqDBCfg) (appCfg.autoMigrate)
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    orgShortId <- askConfig (.selfId)
    modFlowRtWithAuthManagers flowRt appEnv [orgShortId]
  where
    middleware = hashBodyForSignature
    context = verifyPersonAction @(FlowR AppEnv) :. EmptyContext
