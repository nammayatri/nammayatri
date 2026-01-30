{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UnifiedDashboard where

import qualified API.Action.UnifiedDashboard.Fleet as FleetDSL
import qualified API.Action.UnifiedDashboard.Management as ManagementDSL
import qualified API.Action.UnifiedDashboard.Operator as OperatorDSL
import qualified API.Action.UnifiedDashboard.RideBooking as RideBookingDSL
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth (DashboardTokenAuth)

type API =
  "unified-dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" Context.City
    :> ( "fleet" :> FleetDSLAPI
           :<|> "management" :> ManagementDSLAPI
           :<|> "operator" :> OperatorDSLAPI
           :<|> "rideBooking" :> RideBookingDSLAPI
       )

type FleetDSLAPI = DashboardTokenAuth :> FleetDSL.API

type ManagementDSLAPI = DashboardTokenAuth :> ManagementDSL.API

type OperatorDSLAPI = DashboardTokenAuth :> OperatorDSL.API

type RideBookingDSLAPI = DashboardTokenAuth :> RideBookingDSL.API

-- TODO IssueManagement

handler :: FlowServer API
handler =
  \merchantId city -> do
    unifiedDashboardFleetDSLHandler merchantId city
      :<|> unifiedDashboardManagementDSLHandler merchantId city
      :<|> unifiedDashboardOperatorDSLHandler merchantId city
      :<|> unifiedDashboardRideBookingDSLHandler merchantId city

unifiedDashboardFleetDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer FleetDSLAPI
unifiedDashboardFleetDSLHandler merchantId city _auth = FleetDSL.handler merchantId city

unifiedDashboardManagementDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer ManagementDSLAPI
unifiedDashboardManagementDSLHandler merchantId city _auth = ManagementDSL.handler merchantId city

unifiedDashboardOperatorDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer OperatorDSLAPI
unifiedDashboardOperatorDSLHandler merchantId city _auth = OperatorDSL.handler merchantId city

unifiedDashboardRideBookingDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer RideBookingDSLAPI
unifiedDashboardRideBookingDSLHandler merchantId city _auth = RideBookingDSL.handler merchantId city
