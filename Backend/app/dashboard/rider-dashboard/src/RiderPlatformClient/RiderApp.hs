{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module RiderPlatformClient.RiderApp
  ( callRiderApp,
  )
where

import qualified "rider-app" API.Dashboard as BAP
import qualified Dashboard.Common.Booking as Booking
import qualified Dashboard.RiderPlatform.Customer as Customer
import qualified Dashboard.RiderPlatform.Merchant as Merchant
import qualified Dashboard.RiderPlatform.Ride as Ride
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (callAPI)
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

data AppBackendAPIs = AppBackendAPIs
  { customers :: CustomerAPIs,
    bookings :: BookingsAPIs,
    merchant :: MerchantAPIs,
    rides :: RidesAPIs
  }

data CustomerAPIs = CustomerAPIs
  { customerList :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Customer.CustomerListRes,
    customerDelete :: Id Customer.Customer -> Euler.EulerClient APISuccess,
    customerBlock :: Id Customer.Customer -> Euler.EulerClient APISuccess,
    customerUnblock :: Id Customer.Customer -> Euler.EulerClient APISuccess,
    customerInfo :: Id Customer.Customer -> Euler.EulerClient Customer.CustomerInfoRes
  }

newtype BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Booking.StuckBookingsCancelReq -> Euler.EulerClient Booking.StuckBookingsCancelRes
  }

data RidesAPIs = RidesAPIs
  { shareRideInfo :: Id Ride.Ride -> Euler.EulerClient Ride.ShareRideInfoRes,
    rideList :: Maybe Int -> Maybe Int -> Maybe Ride.BookingStatus -> Maybe (ShortId Ride.Ride) -> Maybe Text -> Maybe Text -> Euler.EulerClient Ride.RideListRes
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Merchant.MerchantUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceConfigUpdate :: Merchant.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Merchant.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Merchant.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Merchant.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess
  }

mkAppBackendAPIs :: CheckedShortId DM.Merchant -> Text -> AppBackendAPIs
mkAppBackendAPIs merchantId token = do
  let customers = CustomerAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  let rides = RidesAPIs {..}
  AppBackendAPIs {..}
  where
    customersClient
      :<|> bookingsClient
      :<|> merchantClient
      :<|> ridesClient = clientWithMerchant (Proxy :: Proxy BAP.API') merchantId token

    customerList
      :<|> customerDelete
      :<|> customerBlock
      :<|> customerUnblock
      :<|> customerInfo = customersClient

    stuckBookingsCancel = bookingsClient

    shareRideInfo
      :<|> rideList = ridesClient

    merchantUpdate
      :<|> mapsServiceConfigUpdate
      :<|> mapsServiceUsageConfigUpdate
      :<|> smsServiceConfigUpdate
      :<|> smsServiceUsageConfigUpdate = merchantClient

callRiderApp ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI AppBackendAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  (AppBackendAPIs -> b) ->
  c
callRiderApp merchantId = callServerAPI @_ @m @r APP_BACKEND (mkAppBackendAPIs merchantId) "callRiderApp"
