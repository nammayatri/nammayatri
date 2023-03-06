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
import qualified Dashboard.Common.Booking as Common
import qualified Dashboard.Common.Exotel as Common
import qualified Dashboard.ProviderPlatform.Driver as Common
import qualified Dashboard.ProviderPlatform.Merchant as Common
import qualified Dashboard.ProviderPlatform.Ride as Common
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
  { listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Common.DriverListRes,
    driverActivity :: Euler.EulerClient Common.DriverActivityRes,
    enableDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    disableDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    blockDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    unblockDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    driverLocation :: Maybe Int -> Maybe Int -> Common.DriverIds -> Euler.EulerClient Common.DriverLocationRes,
    driverInfo :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Euler.EulerClient Common.DriverInfoRes,
    deleteDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    unlinkVehicle :: Id Common.Driver -> Euler.EulerClient APISuccess,
    updatePhoneNumber :: Id Common.Driver -> Common.UpdatePhoneNumberReq -> Euler.EulerClient APISuccess,
    addVehicle :: Id Common.Driver -> Common.AddVehicleReq -> Euler.EulerClient APISuccess,
    updateDriverName :: Id Common.Driver -> Common.UpdateDriverNameReq -> Euler.EulerClient APISuccess
  }

data RidesAPIs = RidesAPIs
  { rideList :: Maybe Int -> Maybe Int -> Maybe Common.BookingStatus -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Money -> Euler.EulerClient Common.RideListRes,
    rideStart :: Id Common.Ride -> Common.StartRideReq -> Euler.EulerClient APISuccess,
    rideEnd :: Id Common.Ride -> Common.EndRideReq -> Euler.EulerClient APISuccess,
    rideCancel :: Id Common.Ride -> Common.CancelRideReq -> Euler.EulerClient APISuccess,
    rideInfo :: Id Common.Ride -> Euler.EulerClient Common.RideInfoRes,
    rideSync :: Id Common.Ride -> Euler.EulerClient Common.RideSyncRes
  }

newtype BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Common.StuckBookingsCancelReq -> Euler.EulerClient Common.StuckBookingsCancelRes
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Common.MerchantUpdateReq -> Euler.EulerClient Common.MerchantUpdateRes,
    mapsServiceConfigUpdate :: Common.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Common.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Common.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Common.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess
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
      :<|> rideSync = ridesClient

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
  { exotelHeartbeat :: Common.ExotelHeartbeatReq -> Euler.EulerClient APISuccess
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
