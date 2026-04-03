{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module API.Client.Rider.Management where
import Kernel.Prelude
import Servant
import qualified "rider-app" API.Types.UnifiedDashboard.Management.HealthCheck
import qualified Kernel.Types.Beckn.City
import qualified Domain.Types.Merchant
import qualified Domain.Types.AccessMatrix
import qualified Tools.Auth.Merchant
import qualified Tools.Client
import qualified "rider-app" API.UnifiedDashboard


newtype ManagementAPIs = ManagementAPIs {healthCheckDSL :: API.Types.UnifiedDashboard.Management.HealthCheck.HealthCheckAPIs}
mkManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ManagementAPIs)
mkManagementAPIs merchantId city token = do {let {healthCheckDSL = API.Types.UnifiedDashboard.Management.HealthCheck.mkHealthCheckAPIs healthCheckClientDSL}; (ManagementAPIs {..})}
                     where healthCheckClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.UnifiedDashboard.ManagementDSLAPI) merchantId city token
callManagementAPI :: forall m r b c . Tools.Client.DashboardClient ManagementAPIs m r b c =>
                     (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (ManagementAPIs -> b) -> c)
callManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.AccessMatrix.APP_BACKEND_MANAGEMENT (mkManagementAPIs merchantId city) "callManagementAPI"



