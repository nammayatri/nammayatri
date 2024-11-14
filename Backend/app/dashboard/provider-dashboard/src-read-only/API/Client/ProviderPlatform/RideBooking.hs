{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.RideBooking where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.DriverRegistration
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Maps
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Ride
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data RideBookingAPIs = RideBookingAPIs
  { driverDSL :: API.Types.Dashboard.RideBooking.Driver.DriverAPIs,
    driverRegistrationDSL :: API.Types.Dashboard.RideBooking.DriverRegistration.DriverRegistrationAPIs,
    mapsDSL :: API.Types.Dashboard.RideBooking.Maps.MapsAPIs,
    rideDSL :: API.Types.Dashboard.RideBooking.Ride.RideAPIs,
    volunteerDSL :: API.Types.Dashboard.RideBooking.Volunteer.VolunteerAPIs
  }

mkRideBookingAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> RideBookingAPIs)
mkRideBookingAPIs merchantId city token = do
  let driverDSL = API.Types.Dashboard.RideBooking.Driver.mkDriverAPIs driverClientDSL
  let driverRegistrationDSL = API.Types.Dashboard.RideBooking.DriverRegistration.mkDriverRegistrationAPIs driverRegistrationClientDSL
  let mapsDSL = API.Types.Dashboard.RideBooking.Maps.mkMapsAPIs mapsClientDSL
  let rideDSL = API.Types.Dashboard.RideBooking.Ride.mkRideAPIs rideClientDSL
  let volunteerDSL = API.Types.Dashboard.RideBooking.Volunteer.mkVolunteerAPIs volunteerClientDSL
  (RideBookingAPIs {..})
  where
    driverClientDSL :<|> driverRegistrationClientDSL :<|> mapsClientDSL :<|> rideClientDSL :<|> volunteerClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.RideBookingDSLAPI) merchantId city token

callRideBookingAPI ::
  forall m r b c.
  Tools.Client.DashboardClient RideBookingAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (RideBookingAPIs -> b) -> c)
callRideBookingAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP (mkRideBookingAPIs merchantId city) "callRideBookingAPI"
