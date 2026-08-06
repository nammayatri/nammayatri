{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Provider.Operator.HealthCheck
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Operator
import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Operator.HealthCheck
import qualified Domain.Action.Provider.Operator.HealthCheck
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("healthCheck" :> GetHealthCheckTest)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getHealthCheckTest merchantId city

type GetHealthCheckTest = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'GET_OPERATOR_HEALTH_CHECK_TEST :> API.Types.UnifiedDashboard.Operator.HealthCheck.GetHealthCheckTest)

getHealthCheckTest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
getHealthCheckTest merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.Provider.Operator.HealthCheck.getHealthCheckTest merchantShortId opCity apiTokenInfo
