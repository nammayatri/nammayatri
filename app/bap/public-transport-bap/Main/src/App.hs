{-# LANGUAGE TypeApplications #-}

module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Core.Beckn (logBecknRequest)
import Environment
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.App
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runServerWithHealthCheck)
import Kernel.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Servant (Context (..))
import Tools.Auth (verifyPersonAction)

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "public-transport-bap" <&> configModifier
  appEnv <- buildAppEnv appCfg
  runServerWithHealthCheck appEnv (Proxy @API) handler (middleware appEnv) identity context releaseAppEnv \flowRt -> do
    migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    modFlowRtWithAuthManagers flowRt appEnv [(appCfg.selfId, appCfg.authEntity.uniqueKeyId)]
  where
    middleware appEnv =
      hashBodyForSignature
        >>> supportProxyAuthorization
        >>> logBecknRequest appEnv
    context = verifyPersonAction @(FlowR AppEnv) :. EmptyContext
