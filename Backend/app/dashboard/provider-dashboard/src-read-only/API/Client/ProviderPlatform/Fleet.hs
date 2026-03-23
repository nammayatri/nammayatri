{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.Fleet where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import qualified API.Types.ProviderPlatform.Fleet.Onboarding
import qualified API.Types.ProviderPlatform.Fleet.PayoutAccount
import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data FleetAPIs = FleetAPIs
  { driverDSL :: API.Types.ProviderPlatform.Fleet.Driver.DriverAPIs,
    liveMapDSL :: API.Types.ProviderPlatform.Fleet.LiveMap.LiveMapAPIs,
    onboardingDSL :: API.Types.ProviderPlatform.Fleet.Onboarding.OnboardingAPIs,
    payoutAccountDSL :: API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountAPIs,
    registrationV2DSL :: API.Types.ProviderPlatform.Fleet.RegistrationV2.RegistrationV2APIs
  }

mkFleetAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> FleetAPIs)
mkFleetAPIs merchantId city token = do
  let driverDSL = API.Types.ProviderPlatform.Fleet.Driver.mkDriverAPIs driverClientDSL
  let liveMapDSL = API.Types.ProviderPlatform.Fleet.LiveMap.mkLiveMapAPIs liveMapClientDSL
  let onboardingDSL = API.Types.ProviderPlatform.Fleet.Onboarding.mkOnboardingAPIs onboardingClientDSL
  let payoutAccountDSL = API.Types.ProviderPlatform.Fleet.PayoutAccount.mkPayoutAccountAPIs payoutAccountClientDSL
  let registrationV2DSL = API.Types.ProviderPlatform.Fleet.RegistrationV2.mkRegistrationV2APIs registrationV2ClientDSL
  (FleetAPIs {..})
  where
    driverClientDSL :<|> liveMapClientDSL :<|> onboardingClientDSL :<|> payoutAccountClientDSL :<|> registrationV2ClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.FleetDSLAPI) merchantId city token

callFleetAPI ::
  forall m r b c.
  Tools.Client.DashboardClient FleetAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (FleetAPIs -> b) -> c)
callFleetAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkFleetAPIs merchantId city) "callFleetAPI"
