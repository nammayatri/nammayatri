{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ProviderPlatformClient.StaticOfferDriver
  ( callBecknTransportBPP,
    callStaticOfferDriverAppExotelApi,
  )
where

import "static-offer-driver-app" API.Dashboard as BPP
import qualified Dashboard.Common.Booking as Booking
import qualified Dashboard.Common.Exotel as Exotel
import qualified Dashboard.ProviderPlatform.Driver as Driver
import qualified Dashboard.ProviderPlatform.Merchant as Merchant
import qualified Dashboard.ProviderPlatform.Ride as Ride
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

data BecknTransportAPIs = BecknTransportAPIs
  { drivers :: DriversAPIs,
    rides :: RidesAPIs,
    bookings :: BookingsAPIs,
    merchant :: MerchantAPIs
  }

data DriversAPIs = DriversAPIs
  { listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Euler.EulerClient Driver.DriverListRes,
    driverActivity :: Euler.EulerClient Driver.DriverActivityRes,
    enableDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    disableDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    blockDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unblockDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    driverLocation :: Maybe Int -> Maybe Int -> Driver.DriverIds -> Euler.EulerClient Driver.DriverLocationRes,
    driverInfo :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Euler.EulerClient Driver.DriverInfoRes,
    deleteDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unlinkVehicle :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    updatePhoneNumber :: Id Driver.Driver -> Driver.UpdatePhoneNumberReq -> Euler.EulerClient APISuccess,
    addVehicle :: Id Driver.Driver -> Driver.AddVehicleReq -> Euler.EulerClient APISuccess,
    updateDriverName :: Id Driver.Driver -> Driver.UpdateDriverNameReq -> Euler.EulerClient APISuccess
  }

data RidesAPIs = RidesAPIs
  { rideList :: Maybe Int -> Maybe Int -> Maybe Ride.BookingStatus -> Maybe (ShortId Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Money -> Maybe UTCTime -> Maybe UTCTime -> Euler.EulerClient Ride.RideListRes,
    rideStart :: Id Ride.Ride -> Ride.StartRideReq -> Euler.EulerClient APISuccess,
    rideEnd :: Id Ride.Ride -> Ride.EndRideReq -> Euler.EulerClient APISuccess,
    rideCancel :: Id Ride.Ride -> Ride.CancelRideReq -> Euler.EulerClient APISuccess,
    rideInfo :: Id Ride.Ride -> Euler.EulerClient Ride.RideInfoRes,
    rideSync :: Id Ride.Ride -> Euler.EulerClient Ride.RideSyncRes,
    multipleRideSync :: Ride.MultipleRideSyncReq -> Euler.EulerClient Ride.MultipleRideSyncRes,
    rideRoute :: Id Ride.Ride -> Euler.EulerClient Ride.RideRouteRes
  }

newtype BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Booking.StuckBookingsCancelReq -> Euler.EulerClient Booking.StuckBookingsCancelRes
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Merchant.MerchantUpdateReq -> Euler.EulerClient Merchant.MerchantUpdateRes,
    mapsServiceConfigUpdate :: Merchant.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Merchant.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Merchant.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Merchant.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess
  }

mkBecknTransportAPIs :: CheckedShortId DM.Merchant -> Text -> BecknTransportAPIs
mkBecknTransportAPIs merchantId token = do
  let drivers = DriversAPIs {..}
  let rides = RidesAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  BecknTransportAPIs {..}
  where
    driversClient
      :<|> ridesClient
      :<|> bookingsClient
      :<|> merchantClient = clientWithMerchant (Proxy :: Proxy BPP.API') merchantId token

    listDrivers
      :<|> driverActivity
      :<|> enableDriver
      :<|> disableDriver
      :<|> blockDriver
      :<|> unblockDriver
      :<|> driverLocation
      :<|> driverInfo
      :<|> deleteDriver
      :<|> unlinkVehicle
      :<|> updatePhoneNumber
      :<|> addVehicle
      :<|> updateDriverName = driversClient

    rideList
      :<|> rideStart
      :<|> rideEnd
      :<|> rideCancel
      :<|> rideInfo
      :<|> rideSync
      :<|> multipleRideSync
      :<|> rideRoute = ridesClient

    stuckBookingsCancel = bookingsClient

    merchantUpdate
      :<|> mapsServiceConfigUpdate
      :<|> mapsServiceUsageConfigUpdate
      :<|> smsServiceConfigUpdate
      :<|> smsServiceUsageConfigUpdate = merchantClient

callBecknTransportBPP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI BecknTransportAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  (BecknTransportAPIs -> b) ->
  c
callBecknTransportBPP merchantId = callServerAPI @_ @m @r BECKN_TRANSPORT (mkBecknTransportAPIs merchantId) "callBecknTransportBPP"

newtype ExotelAPIs = ExotelAPIs
  { exotelHeartbeat :: Exotel.ExotelHeartbeatReq -> Euler.EulerClient APISuccess
  }

mkStaticOfferDriverAppExotelAPIs :: Text -> ExotelAPIs
mkStaticOfferDriverAppExotelAPIs token = do
  ExotelAPIs {..}
  where
    exotelHeartbeat = Euler.client (Proxy :: Proxy BPP.ExotelAPI) token

callStaticOfferDriverAppExotelApi ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI ExotelAPIs m r b c
  ) =>
  (ExotelAPIs -> b) ->
  c
callStaticOfferDriverAppExotelApi = callServerAPI @_ @m @r BECKN_TRANSPORT mkStaticOfferDriverAppExotelAPIs "callStaticOfferDriverAppExotelApi"
