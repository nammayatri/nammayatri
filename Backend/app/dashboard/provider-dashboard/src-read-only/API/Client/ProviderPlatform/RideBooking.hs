{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.RideBooking where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.RideBooking.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

newtype RideBookingAPIs = RideBookingAPIs {driverDSL :: API.Types.ProviderPlatform.RideBooking.Driver.DriverAPIs}

mkRideBookingAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> RideBookingAPIs)
mkRideBookingAPIs merchantId city token = do let { driverDSL = API.Types.ProviderPlatform.RideBooking.Driver.mkDriverAPIs driverClientDSL }; (RideBookingAPIs {..})
  where
    driverClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.RideBookingDSLAPI) merchantId city token

callRideBookingAPI ::
  forall m r b c.
  Tools.Client.DashboardClient RideBookingAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (RideBookingAPIs -> b) -> c)
callRideBookingAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP (mkRideBookingAPIs merchantId city) "callRideBookingAPI"
