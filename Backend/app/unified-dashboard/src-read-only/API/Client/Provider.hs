{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.Provider where

import qualified "dynamic-offer-driver-app" API.Types.Provider.Person
import qualified "dynamic-offer-driver-app" API.UnifiedDashboard
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified Tools.Auth.Merchant
import qualified Tools.Client

newtype ProviderAPIs = ProviderAPIs {personDSL :: API.Types.Provider.Person.PersonAPIs}

mkProviderAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ProviderAPIs)
mkProviderAPIs merchantId city token = do let { personDSL = API.Types.Provider.Person.mkPersonAPIs personClientDSL }; (ProviderAPIs {..})
  where
    personClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.UnifiedDashboard.ProviderDSLAPI) merchantId city token

callProviderAPI ::
  forall m r b c.
  Tools.Client.DashboardClient ProviderAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (ProviderAPIs -> b) -> c)
callProviderAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.AccessMatrix.DRIVER_OFFER_BPP_MANAGEMENT (mkProviderAPIs merchantId city) "callProviderAPI"
