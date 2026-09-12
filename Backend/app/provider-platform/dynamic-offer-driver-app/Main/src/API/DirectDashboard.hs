{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Dashboard routes served directly to an operator, bypassing
-- provider-dashboard.
--
-- Runs alongside 'API.Dashboard' rather than replacing it. That tree is still
-- reached through the proxy and is also the type provider-dashboard derives its
-- client from, so it must keep its current shape; routes move across here one
-- at a time and the proxied path keeps working throughout.
--
-- Each route here authenticates the operator's own session and enforces the
-- capability registered for that specific endpoint -- as opposed to
-- 'Tools.Auth.DashboardTokenAuth', which is per tree and compares one shared
-- static token.
--
-- Public URLs are unchanged: ingress maps the dashboard host's paths onto this
-- prefix, so the frontend does not move when a route migrates.
module API.DirectDashboard
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.AppManagement as AppManagementDSL
import qualified API.Action.DashboardAuth.Fleet as FleetDSL
import qualified API.Action.DashboardAuth.IssueManagement as IssueManagementDSL
import qualified API.Action.DashboardAuth.Management as ManagementDSL
import qualified API.Action.DashboardAuth.Operator as OperatorDSL
import qualified API.Action.DashboardAuth.RideBooking as RideBookingDSL
import qualified Domain.Types.Merchant as DM
import Environment
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant

type API =
  "direct-dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" Context.City
    :> ( AppManagementDSL.API
           :<|> FleetDSL.API
           :<|> OperatorDSL.API
           :<|> RideBookingDSL.API
           :<|> ManagementDSL.API
           :<|> IssueManagementDSL.API
       )

handler :: FlowServer API
handler merchantId city =
  AppManagementDSL.handler merchantId city
    :<|> FleetDSL.handler merchantId city
    :<|> OperatorDSL.handler merchantId city
    :<|> RideBookingDSL.handler merchantId city
    :<|> ManagementDSL.handler merchantId city
    :<|> IssueManagementDSL.handler merchantId city
