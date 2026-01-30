{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UnifiedDashboard.Operator.HealthCheck
  ( API.Types.UnifiedDashboard.Operator.HealthCheck.API,
    handler,
  )
where

import qualified API.Types.UnifiedDashboard.Operator.HealthCheck
import qualified Domain.Action.UnifiedDashboard.Operator.HealthCheck
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.UnifiedDashboard.Operator.HealthCheck.API)
handler merchantId city = getHealthCheckTest merchantId city

getHealthCheckTest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
getHealthCheckTest a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.UnifiedDashboard.Operator.HealthCheck.getHealthCheckTest a2 a1
