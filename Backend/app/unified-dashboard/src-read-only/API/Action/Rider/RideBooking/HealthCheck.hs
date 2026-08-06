{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Rider.RideBooking.HealthCheck
  ( API,
    handler,
  )
where

import qualified "rider-app" API.Types.UnifiedDashboard.RideBooking
import qualified "rider-app" API.Types.UnifiedDashboard.RideBooking.HealthCheck
import qualified Domain.Action.Rider.RideBooking.HealthCheck
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

type GetHealthCheckTest = (ApiAuth 'APP_BACKEND_MANAGEMENT 'GET_RIDE_BOOKING_HEALTH_CHECK_TEST :> API.Types.UnifiedDashboard.RideBooking.HealthCheck.GetHealthCheckTest)

getHealthCheckTest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
getHealthCheckTest merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.Rider.RideBooking.HealthCheck.getHealthCheckTest merchantShortId opCity apiTokenInfo
