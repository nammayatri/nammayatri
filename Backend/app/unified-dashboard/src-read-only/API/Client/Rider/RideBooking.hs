{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.Rider.RideBooking where

import qualified "rider-app" API.Types.UnifiedDashboard.RideBooking.HealthCheck
import qualified "rider-app" API.UnifiedDashboard
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified Tools.Auth.Merchant
import qualified Tools.Client

newtype RideBookingAPIs = RideBookingAPIs {healthCheckDSL :: API.Types.UnifiedDashboard.RideBooking.HealthCheck.HealthCheckAPIs}

mkRideBookingAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> RideBookingAPIs)
mkRideBookingAPIs merchantId city token = do let { healthCheckDSL = API.Types.UnifiedDashboard.RideBooking.HealthCheck.mkHealthCheckAPIs healthCheckClientDSL }; (RideBookingAPIs {..})
  where
    healthCheckClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.UnifiedDashboard.RideBookingDSLAPI) merchantId city token

callRideBookingAPI ::
  forall m r b c.
  Tools.Client.DashboardClient RideBookingAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (RideBookingAPIs -> b) -> c)
callRideBookingAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.AccessMatrix.APP_BACKEND_MANAGEMENT (mkRideBookingAPIs merchantId city) "callRideBookingAPI"
