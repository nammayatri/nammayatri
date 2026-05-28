{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.Conductor where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.Conductor.Registration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

newtype ConductorAPIs = ConductorAPIs {registrationDSL :: API.Types.ProviderPlatform.Conductor.Registration.RegistrationAPIs}

mkConductorAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ConductorAPIs)
mkConductorAPIs merchantId city token = do let { registrationDSL = API.Types.ProviderPlatform.Conductor.Registration.mkRegistrationAPIs registrationClientDSL }; (ConductorAPIs {..})
  where
    registrationClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.ConductorDSLAPI) merchantId city token

callConductorAPI ::
  forall m r b c.
  Tools.Client.DashboardClient ConductorAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (ConductorAPIs -> b) -> c)
callConductorAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkConductorAPIs merchantId city) "callConductorAPI"
