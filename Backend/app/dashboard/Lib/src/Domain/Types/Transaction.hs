{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Transaction where

import qualified "dynamic-offer-driver-app" API.Dashboard.Overlay as BPP
import qualified "rider-app" API.Dashboard.RideBooking.Booking as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Cancel as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Confirm as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Frontend as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Maps as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Profile as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Registration as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Search as BAP
import qualified "rider-app" API.Dashboard.RideBooking.Select as BAP
import qualified "dynamic-offer-driver-app" API.Dashboard.Subscription as BPP
import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import qualified "dashboard-helper-api" Dashboard.Common.Exotel as Common
import qualified "dashboard-helper-api" Dashboard.Common.Issue as Common
import qualified "dashboard-helper-api" Dashboard.Common.Merchant as Common
import qualified "dashboard-helper-api" Dashboard.Common.Message as Common
import qualified "dashboard-helper-api" Dashboard.Common.SpecialZone as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.DriverReferral as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Volunteer as Common
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Customer as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import Kernel.Types.Id

-- request is raw Text here, because if some field will be changed, we can't parse it
data Transaction = Transaction
  { id :: Id Transaction,
    requestorId :: Maybe (Id DP.Person),
    serverName :: Maybe DSN.ServerName,
    merchantId :: Maybe (Id DM.Merchant), -- will be Nothing for admin apis
    endpoint :: Endpoint,
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    request :: Maybe Text,
    response :: Maybe Text,
    responseError :: Maybe Text,
    createdAt :: UTCTime
  }

data Endpoint
  = RideAPI Common.RideEndpoint
  | BookingAPI Common.BookingEndpoint
  | DriverAPI Common.DriverEndpoint
  | DriverReferralAPI Common.ReferralEndpoint
  | DriverRegistrationAPI Common.DriverRegistrationEndpoint
  | MerchantAPI Common.MerchantEndpoint
  | CustomerAPI Common.CustomerEndpoint
  | MessageAPI Common.MessageEndpoint
  | ExotelAPI Common.ExotelEndpoint
  | IssueAPI Common.IssueEndpoint
  | VolunteerAPI Common.VolunteerEndpoint
  | RegistrationAPI BAP.RegistrationEndPoint
  | SearchAPI BAP.RideSearchEndPoint
  | SelectAPI BAP.RideEstimatesEndPoint
  | ConfirmAPI BAP.RideConfirmEndPoint
  | RBooking BAP.RideBookingEndPoint
  | ProfileAPI BAP.ProfileEndPoint
  | MapsAPI BAP.MapEndPoints
  | FlowStatusAPI BAP.RideNotifyEventEndPoint
  | CancelAPI BAP.RideCancelEndPoint
  | SpecialZoneAPI Common.SpecialZoneEndpoint
  | SubscriptionAPI BPP.SubscriptionEndpoint
  | OverlayAPI BPP.OverlayEndpoint
  deriving (Show, Read)
