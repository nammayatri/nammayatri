{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.Fleet where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

newtype FleetAPIs = FleetAPIs {driverDSL :: API.Types.ProviderPlatform.Fleet.Driver.DriverAPIs}

mkFleetAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> FleetAPIs)
mkFleetAPIs merchantId city token = do let { driverDSL = API.Types.ProviderPlatform.Fleet.Driver.mkDriverAPIs driverClientDSL }; (FleetAPIs {..})
  where
    driverClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.FleetDSLAPI) merchantId city token

callFleetAPI ::
  forall m r b c.
  Tools.Client.DashboardClient FleetAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (FleetAPIs -> b) -> c)
callFleetAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkFleetAPIs merchantId city) "callFleetAPI"
