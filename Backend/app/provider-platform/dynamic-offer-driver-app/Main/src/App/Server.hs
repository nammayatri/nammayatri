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
import API.UI.Issue (castMOCityByMerchantShortIdAndCity)
import App.CrossCloudProxy (crossCloudProxy)
import Environment
import EulerHS.Prelude
import qualified IssueManagement.Common as IssueCommon
import IssueManagement.Tools.LookupWhitelist (issueLookupGuard)
import Kernel.Tools.Metrics.Init
import qualified Kernel.Types.App as Fields
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Monitoring.Prometheus.Servant ()
import qualified Kernel.Utils.Servant.Server as BU
import qualified Network.HTTP.Client as Http
import Servant
import Storage.Beam.IssueManagement ()
import Tools.Auth
import qualified "lib-dashboard" Tools.Auth.DashboardLoginFlow as DashboardLogin
import Tools.Auth.DashboardUserAuth (verifyDashboardUserAction)
import "lib-dashboard" Tools.Servant.HeaderAuth (guardVerification)

run :: Http.Manager -> Env -> Application
run proxyManager = withModifiedEnv' driverOfferAPI $ \modifiedEnv ->
  BU.run driverOfferAPI (driverOfferServer modifiedEnv.appEnv) context modifiedEnv
    & crossCloudProxy proxyManager modifiedEnv
    & logRequestAndResponse' modifiedEnv
    -- & logBecknRequest modifiedEnv
    & addServantInfo modifiedEnv.appEnv.version driverOfferAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. verifyDashboardAction @(FlowR AppEnv)
        :. guardVerification issueLookupCheck (verifyDashboardUserAction @(FlowR AppEnv))
        -- Serves lib-dashboard's login tree: resolves the operator's session
        -- against the dashboard database rather than this server's own.
        :. DashboardLogin.dashboardLoginVerifyAction @(FlowR AppEnv)
        :. EmptyContext
    -- Chat lookups (requests carrying the issue lookup token) may only reach
    -- endpoints whitelisted in this app's issue_config for the city.
    issueLookupCheck (_, endpointId) = issueLookupGuard castMOCityByMerchantShortIdAndCity IssueCommon.DRIVER endpointId
