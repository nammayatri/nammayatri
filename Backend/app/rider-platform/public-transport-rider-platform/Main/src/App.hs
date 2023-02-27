{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Beckn.Core (logBecknRequest)
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
  appCfg <- readDhallConfigDefault "public-transport-rider-platform" <&> configModifier
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
