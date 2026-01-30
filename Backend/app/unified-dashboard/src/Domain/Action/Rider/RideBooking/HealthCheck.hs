{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Rider.RideBooking.HealthCheck (getHealthCheckTest) where

import qualified API.Client.Rider.RideBooking
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getHealthCheckTest :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
getHealthCheckTest merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.Rider.RideBooking.callRideBookingAPI checkedMerchantId opCity (.healthCheckDSL.getHealthCheckTest)
