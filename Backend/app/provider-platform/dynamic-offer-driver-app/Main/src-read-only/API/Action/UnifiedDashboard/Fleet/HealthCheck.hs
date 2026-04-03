{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UnifiedDashboard.Fleet.HealthCheck 
( API.Types.UnifiedDashboard.Fleet.HealthCheck.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UnifiedDashboard.Fleet.HealthCheck
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.APISuccess
import qualified API.Types.UnifiedDashboard.Fleet.HealthCheck



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.UnifiedDashboard.Fleet.HealthCheck.API)
handler merchantId city = getHealthCheckTest merchantId city
getHealthCheckTest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
getHealthCheckTest a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.UnifiedDashboard.Fleet.HealthCheck.getHealthCheckTest a2 a1



