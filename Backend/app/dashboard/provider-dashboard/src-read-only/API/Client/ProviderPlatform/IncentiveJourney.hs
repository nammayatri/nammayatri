{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.IncentiveJourney where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

newtype IncentiveJourneyAPIs = IncentiveJourneyAPIs {incentiveJourneyDSL :: API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyAPIs}

mkIncentiveJourneyAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> IncentiveJourneyAPIs)
mkIncentiveJourneyAPIs merchantId city token = do
  let incentiveJourneyDSL = API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.mkIncentiveJourneyAPIs incentiveJourneyClientDSL
  (IncentiveJourneyAPIs {..})
  where
    incentiveJourneyClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.IncentiveJourneyDSLAPI) merchantId city token

callIncentiveJourneyAPI ::
  forall m r b c.
  Tools.Client.DashboardClient IncentiveJourneyAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (IncentiveJourneyAPIs -> b) -> c)
callIncentiveJourneyAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkIncentiveJourneyAPIs merchantId city) "callIncentiveJourneyAPI"
