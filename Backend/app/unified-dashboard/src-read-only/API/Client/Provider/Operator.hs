{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module API.Client.Provider.Operator where
import Kernel.Prelude
import Servant
import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Operator.HealthCheck
import qualified Kernel.Types.Beckn.City
import qualified Domain.Types.Merchant
import qualified Domain.Types.AccessMatrix
import qualified Tools.Auth.Merchant
import qualified Tools.Client
import qualified "dynamic-offer-driver-app" API.UnifiedDashboard


newtype OperatorAPIs = OperatorAPIs {healthCheckDSL :: API.Types.UnifiedDashboard.Operator.HealthCheck.HealthCheckAPIs}
mkOperatorAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> OperatorAPIs)
mkOperatorAPIs merchantId city token = do {let {healthCheckDSL = API.Types.UnifiedDashboard.Operator.HealthCheck.mkHealthCheckAPIs healthCheckClientDSL}; (OperatorAPIs {..})}
                   where healthCheckClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.UnifiedDashboard.OperatorDSLAPI) merchantId city token
callOperatorAPI :: forall m r b c . Tools.Client.DashboardClient OperatorAPIs m r b c =>
                   (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (OperatorAPIs -> b) -> c)
callOperatorAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.AccessMatrix.DRIVER_OFFER_BPP_MANAGEMENT (mkOperatorAPIs merchantId city) "callOperatorAPI"



