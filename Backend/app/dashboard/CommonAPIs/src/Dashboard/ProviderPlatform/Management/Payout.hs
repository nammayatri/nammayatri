module Dashboard.ProviderPlatform.Management.Payout (validatePayoutSchedulerReq) where

import API.Types.ProviderPlatform.Management.Endpoints.Payout
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Predicate
import Kernel.Utils.Validation

validatePayoutSchedulerReq :: Validate PayoutSchedulerReq
validatePayoutSchedulerReq PayoutSchedulerReq {..} =
  sequenceA_
    [ validateField "driversInBatch" driversInBatch $ InMaybe $ Min @Int 1,
      validateField "eachDriverDelayMs" eachDriverDelayMs $ InMaybe $ Min @Milliseconds 0,
      validateField "batchDelayS" batchDelayS $ InMaybe $ Min @Seconds 0
    ]
