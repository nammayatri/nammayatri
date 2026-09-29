{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One city's job that asks HDFC about that city's bulk payout batches in flight. The loop lives in
-- "Lib.Payment.Payout.Bulk.StatusCheck"; this is the driver app's job handler for it.
module SharedLogic.Allocator.Jobs.Payout.BulkPayoutStatusCheck (runBulkPayoutStatusCheck) where

import qualified Domain.Action.UI.Payout as UIPayout
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Payout.Bulk.StatusCheck as Bulk
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Payout.Bulk.Driver (driverBulkHandle)
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..))

runBulkPayoutStatusCheck ::
  ( UIPayout.PayoutSettlementFlow m r,
    JobCreator r m
  ) =>
  Job 'BulkPayoutStatusCheck ->
  m ExecutionResult
runBulkPayoutStatusCheck Job {id, jobInfo} =
  withLogTag ("JobId-" <> id.getId) $ do
    let cityId = jobInfo.jobData.merchantOperatingCityId.getId
    -- Idle wait and batch limit come from the city's wallet payout config (the only bulk category).
    -- A missing or bad config must not stop the job, so it falls back to 5 minutes and 50 batches.
    mbConfig <-
      withTryCatch "BulkPayoutStatusCheck:scheduledPayoutConfig" $
        getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = cityId, isEnabled = Nothing, payoutCategory = Just DPayment.DRIVER_WALLET_TRANSACTION}) Nothing
    let mbScheduledConfig = either (const Nothing) identity mbConfig
        idleInterval = fromIntegral (60 * max 1 (fromMaybe 5 (mbScheduledConfig >>= (.bulkStatusCheckIntervalMinutes))))
        batchLimit = max 1 (min 500 (fromMaybe 50 (mbScheduledConfig >>= (.bulkStatusCheckBatchLimit))))
    Bulk.runCityStatusCheckJob driverBulkHandle cityId idleInterval batchLimit
