{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard where

import qualified API.Action.Dashboard.AppManagement as AppManagementDSL
import qualified API.Action.Dashboard.IssueManagement as IssueManagementDSL
import qualified API.Action.Dashboard.Management as ManagementDSL
import qualified API.Action.Dashboard.RideBooking as RideBookingDSL
import qualified API.Dashboard.Exotel as Exotel
import qualified Domain.Types.Merchant as DM
import Environment
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth (DashboardTokenAuth)

type APIV2 =
  "dashboard"
    :> ( Capture "merchantId" (ShortId DM.Merchant)
           :> Capture "city" Context.City
           :> ( ManagementDSLAPI
                  :<|> AppManagementDSLAPI
                  :<|> IssueManagementDSLAPI
                  :<|> RideBookingDSLAPI
              )
       )
    :<|> ExotelAPI

type ManagementDSLAPI = DashboardTokenAuth :> ManagementDSL.API

type AppManagementDSLAPI = DashboardTokenAuth :> AppManagementDSL.API

type IssueManagementDSLAPI = DashboardTokenAuth :> IssueManagementDSL.API

type RideBookingDSLAPI = DashboardTokenAuth :> "rideBooking" :> RideBookingDSL.API

handlerV2 :: FlowServer APIV2
handlerV2 =
  ( \merchantId city ->
      managementDSLHandler merchantId city
        :<|> appManagementDSLHandler merchantId city
        :<|> issueManagementDSLHandler merchantId city
        :<|> rideBookingDSLHandler merchantId city
  )
    :<|> exotelHandler

managementDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer ManagementDSLAPI
managementDSLHandler merchantId city _auth = ManagementDSL.handler merchantId city

appManagementDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer AppManagementDSLAPI
appManagementDSLHandler merchantId city _auth = AppManagementDSL.handler merchantId city

issueManagementDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer IssueManagementDSLAPI
issueManagementDSLHandler merchantId city _auth = IssueManagementDSL.handler merchantId city

rideBookingDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer RideBookingDSLAPI
rideBookingDSLHandler merchantId city _auth = RideBookingDSL.handler merchantId city

type ExotelAPI =
  DashboardTokenAuth
    :> Exotel.API

exotelHandler :: FlowServer ExotelAPI
exotelHandler _dashboard =
  Exotel.handler
