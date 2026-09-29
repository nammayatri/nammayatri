{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- NOTE (reviewer, remove before merge): new file (main has no bulk payout code). Handler of the BulkPayoutStatusCheck
--   job (registered in Allocator/src/App.hs). HDFC has no webhook and no per-order status API, so this job asking HDFC
--   about each batch is the only way to learn the outcome. One job per city (city in the job data), created only when a
--   batch is opened (lib openBulkBatch -> ensureCityJob -> driverBulkHandle.createCityStatusCheckJob). Each run checks
--   at most batchLimit of its city's due batches (Lib.Payment.Payout.Bulk.StatusCheck.runCityStatusCheckJob) and
--   reschedules itself: 5 s later if batches were due, else (and after an error) after the city's interval.
--   Bulk-only: Juspay/Stripe keep main's per-order PayoutStatusCheck job; this job only picks up payout_batch rows.

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
    -- One city's job: checks that city's batches.
    let cityId = jobInfo.jobData.merchantOperatingCityId.getId
    -- How long to wait when a run finds nothing due, and how many batches one run takes, from the
    -- city's wallet payout config (the only category paid in bulk). A missing or unreadable config
    -- must not stop the city's job, so it falls back to every 5 minutes and 50 batches.
    mbConfig <-
      withTryCatch "BulkPayoutStatusCheck:scheduledPayoutConfig" $
        getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = cityId, isEnabled = Nothing, payoutCategory = Just DPayment.DRIVER_WALLET_TRANSACTION}) Nothing
    let mbScheduledConfig = either (const Nothing) identity mbConfig
        idleInterval = fromIntegral (60 * max 1 (fromMaybe 5 (mbScheduledConfig >>= (.bulkStatusCheckIntervalMinutes))))
        batchLimit = max 1 (min 500 (fromMaybe 50 (mbScheduledConfig >>= (.bulkStatusCheckBatchLimit))))
    Bulk.runCityStatusCheckJob driverBulkHandle cityId idleInterval batchLimit
