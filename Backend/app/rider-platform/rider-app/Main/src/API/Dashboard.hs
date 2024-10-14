{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard where

import qualified API.Action.Dashboard.Management as ManagementDSL
-- import qualified API.Action.Dashboard.RideBooking as RideBookingDSL
import qualified API.Dashboard.Customer as Customer
import qualified API.Dashboard.Exotel as Exotel
import qualified API.Dashboard.HotSpot as HotSpot
import qualified API.Dashboard.Issue as Issue
import qualified API.Dashboard.IssueList as IssueList
import qualified API.Dashboard.Ride as Ride
import qualified API.Dashboard.RideBooking as RideBookings
import qualified API.Dashboard.Tickets as Tickets
import qualified Domain.Types.Merchant as DM
import Environment
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth (DashboardTokenAuth)

-- TODO :: Deprecated, Remove after successful deployment
type API =
  "dashboard"
    :> ( Capture "merchantId" (ShortId DM.Merchant)
           :> ( OperationsAPI
                  :<|> RideBookingAPI
              )
       )
    :<|> ExotelAPI

type APIV2 =
  "dashboard"
    :> ( Capture "merchantId" (ShortId DM.Merchant)
           :> Capture "city" Context.City
           :> ( OperationsAPI
                  :<|> RideBookingAPI
                  :<|> ManagementDSLAPI
                  -- :<|> RideBookingDSLAPI
              )
       )
    :<|> ExotelAPI

type OperationsAPI =
  DashboardTokenAuth
    :> ( Customer.API
           :<|> Ride.API
           :<|> IssueList.API
           :<|> Issue.API
           :<|> Tickets.API
           :<|> HotSpot.API
       )

type RideBookingAPI =
  DashboardTokenAuth
    :> RideBookings.API

type ManagementDSLAPI = DashboardTokenAuth :> ManagementDSL.API

-- type RideBookingDSLAPI = DashboardTokenAuth :> ManagementDSL.API

-- TODO :: Deprecated, Remove after successful deployment
handler :: FlowServer API
handler =
  ( \merchantId -> do
      let city = getCity merchantId.getShortId
      operationHandler merchantId city
        :<|> rideBookingHandler merchantId city
  )
    :<|> exotelHandler
  where
    getCity = \case
      "NAMMA_YATRI" -> Context.Bangalore
      "YATRI" -> Context.Kochi
      "JATRI_SAATHI" -> Context.Kolkata
      _ -> Context.AnyCity

handlerV2 :: FlowServer APIV2
handlerV2 =
  ( \merchantId city ->
      operationHandler merchantId city
        :<|> rideBookingHandler merchantId city
        :<|> managementDSLHandler merchantId city
        -- :<|> rideBookingDSLHandler merchantId city
  )
    :<|> exotelHandler

operationHandler :: ShortId DM.Merchant -> Context.City -> FlowServer OperationsAPI
operationHandler merchantId city _auth = do
  Customer.handler merchantId city
    :<|> Ride.handler merchantId
    :<|> IssueList.handler merchantId
    :<|> Issue.handler merchantId city
    :<|> Tickets.handler merchantId
    :<|> HotSpot.handler merchantId

rideBookingHandler :: ShortId DM.Merchant -> Context.City -> FlowServer RideBookingAPI
rideBookingHandler merchantId _ _auth = RideBookings.handler merchantId

managementDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer ManagementDSLAPI
managementDSLHandler merchantId city _auth = ManagementDSL.handler merchantId city

-- rideBookingDSLHandler :: ShortId DM.Merchant -> Context.City -> FlowServer RideBookingDSLAPI
-- rideBookingDSLHandler merchantId city _auth = ManagementDSL.handler merchantId city

type ExotelAPI =
  DashboardTokenAuth
    :> Exotel.API

exotelHandler :: FlowServer ExotelAPI
exotelHandler _dashboard =
  Exotel.handler
