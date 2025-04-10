{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.Operator where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified API.Types.ProviderPlatform.Operator.Registration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data OperatorAPIs = OperatorAPIs
  { driverDSL :: API.Types.ProviderPlatform.Operator.Driver.DriverAPIs,
    fleetManagementDSL :: API.Types.ProviderPlatform.Operator.FleetManagement.FleetManagementAPIs,
    registrationDSL :: API.Types.ProviderPlatform.Operator.Registration.RegistrationAPIs
  }

mkOperatorAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> OperatorAPIs)
mkOperatorAPIs merchantId city token = do
  let driverDSL = API.Types.ProviderPlatform.Operator.Driver.mkDriverAPIs driverClientDSL
  let fleetManagementDSL = API.Types.ProviderPlatform.Operator.FleetManagement.mkFleetManagementAPIs fleetManagementClientDSL
  let registrationDSL = API.Types.ProviderPlatform.Operator.Registration.mkRegistrationAPIs registrationClientDSL
  (OperatorAPIs {..})
  where
    driverClientDSL :<|> fleetManagementClientDSL :<|> registrationClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.OperatorDSLAPI) merchantId city token

callOperatorAPI ::
  forall m r b c.
  Tools.Client.DashboardClient OperatorAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (OperatorAPIs -> b) -> c)
callOperatorAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkOperatorAPIs merchantId city) "callOperatorAPI"
