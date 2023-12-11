{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Server where

import API
-- import Beckn.Core (logBecknRequest)
import Environment
import EulerHS.Prelude
import Kernel.Tools.Metrics.Init
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Monitoring.Prometheus.Servant ()
import qualified Kernel.Utils.Servant.Server as BU
import Servant
import Tools.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run driverOfferAPI driverOfferServer context modifiedEnv
    & logRequestAndResponse modifiedEnv
    -- & logBecknRequest modifiedEnv
    & addServantInfo modifiedEnv.appEnv.version (Just modifiedEnv.appEnv.criticalAPIs) driverOfferAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. verifyDashboardAction @(FlowR AppEnv)
        :. EmptyContext
