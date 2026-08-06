{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.Provider.Fleet where

import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Fleet.HealthCheck
import qualified "dynamic-offer-driver-app" API.UnifiedDashboard
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified Tools.Auth.Merchant
import qualified Tools.Client

newtype FleetAPIs = FleetAPIs {healthCheckDSL :: API.Types.UnifiedDashboard.Fleet.HealthCheck.HealthCheckAPIs}

mkFleetAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> FleetAPIs)
mkFleetAPIs merchantId city token = do let { healthCheckDSL = API.Types.UnifiedDashboard.Fleet.HealthCheck.mkHealthCheckAPIs healthCheckClientDSL }; (FleetAPIs {..})
  where
    healthCheckClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.UnifiedDashboard.FleetDSLAPI) merchantId city token

callFleetAPI ::
  forall m r b c.
  Tools.Client.DashboardClient FleetAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (FleetAPIs -> b) -> c)
callFleetAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.AccessMatrix.DRIVER_OFFER_BPP_MANAGEMENT (mkFleetAPIs merchantId city) "callFleetAPI"
