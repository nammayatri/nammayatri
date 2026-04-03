{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module API.Client.Provider.Management where
import Kernel.Prelude
import Servant
import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Management.HealthCheck
import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Management.Person
import qualified Kernel.Types.Beckn.City
import qualified Domain.Types.Merchant
import qualified Domain.Types.AccessMatrix
import qualified Tools.Auth.Merchant
import qualified Tools.Client
import qualified "dynamic-offer-driver-app" API.UnifiedDashboard


data ManagementAPIs = ManagementAPIs {healthCheckDSL :: API.Types.UnifiedDashboard.Management.HealthCheck.HealthCheckAPIs, personDSL :: API.Types.UnifiedDashboard.Management.Person.PersonAPIs}
mkManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ManagementAPIs)
mkManagementAPIs merchantId city token = do {let {healthCheckDSL = API.Types.UnifiedDashboard.Management.HealthCheck.mkHealthCheckAPIs healthCheckClientDSL};
                                             let {personDSL = API.Types.UnifiedDashboard.Management.Person.mkPersonAPIs personClientDSL};
                                             (ManagementAPIs {..})}
                     where healthCheckClientDSL :<|> personClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.UnifiedDashboard.ManagementDSLAPI) merchantId city token
callManagementAPI :: forall m r b c . Tools.Client.DashboardClient ManagementAPIs m r b c =>
                     (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (ManagementAPIs -> b) -> c)
callManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.AccessMatrix.DRIVER_OFFER_BPP_MANAGEMENT (mkManagementAPIs merchantId city) "callManagementAPI"



