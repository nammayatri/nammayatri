{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.SchedulePayoutVendorSettlementJobs where

import qualified Data.List as List
-- import qualified Domain.Types.Merchant as DM
-- import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
-- import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.PayoutSplitConfig as QPSC

-- Master Job: Creates per-vendor instance scheduler job entries
-- by querying PayoutSplitConfig table for all unique vendorIds
schedulePayoutVendorSettlementJobs ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    JobCreatorEnv r
  ) =>
  Job 'VendorPayoutSettlementMasterJob ->
  m ExecutionResult
schedulePayoutVendorSettlementJobs Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      startTime = jobData.startTime
      endTime = jobData.endTime
      merchantId = jobData.merchantId

  logInfo $ "Starting Vendor Payout Settlement Master Job for merchantOpCityId: " <> merchantOpCityId.getId

  -- Get all PayoutSplitConfigs for this city and extract unique vendor IDs
  payoutConfigs <- QPSC.findAllDistinctPayoutVendorsByCity merchantOpCityId
  let vendors = List.nub $ map (.vendorId) payoutConfigs

  logInfo $ "Found " <> show (length vendors) <> " vendors to process"

  -- Create per-vendor cash collected jobs (Step 1)
  forM_ vendors $ \vendorId -> do
    logInfo $ "Scheduling vendor payout settlement cash collected job for vendorId: " <> vendorId
    createJobIn @_ @'VendorPayoutSettlementCashCollected (Just merchantId) (Just merchantOpCityId) 0 $
      VendorPayoutSettlementCashCollectedJobData
        { merchantId = merchantId,
          merchantOperatingCityId = merchantOpCityId,
          vendorId = vendorId,
          startTime = startTime,
          endTime = endTime
        }
    pure ()

  logInfo "Vendor Payout Settlement Master Job completed successfully"
  return Complete
