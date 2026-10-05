{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.FarePolicy.DeleteUnreferencedFarePolicies
  ( deleteUnreferencedFarePolicies,
    scheduleDeleteUnreferencedFarePolicies,
  )
where

import qualified Data.List as DL
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), DeleteUnreferencedFarePoliciesJobData (..), SchedulerJobFlow)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.FarePolicy as CQFP
import qualified Storage.Queries.ConditionalCharges as QCC
import qualified Storage.Queries.FareProduct as SQF

-- | Searches that read an old FareProduct just before it was replaced still load its
-- FarePolicy a moment later. Deleting the policy straight away makes them fail with
-- NoFarePolicy, so the delete runs as a job after this delay.
deleteDelay :: NominalDiffTime
deleteDelay = 60

scheduleDeleteUnreferencedFarePolicies ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, CoreMetrics m, SchedulerJobFlow r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  [Id DFP.FarePolicy] ->
  m ()
scheduleDeleteUnreferencedFarePolicies merchantId merchantOpCityId fpIds =
  unless (null fpIds) $
    createJobIn @_ @'DeleteUnreferencedFarePolicies (Just merchantId) (Just merchantOpCityId) deleteDelay $
      DeleteUnreferencedFarePoliciesJobData {farePolicyIds = DL.nub fpIds}

-- | Deletes each policy (with its conditional charges) only if no FareProduct, in any
-- city or merchant, references it any more. The check runs now, not at schedule time.
deleteUnreferencedFarePolicies ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Job 'DeleteUnreferencedFarePolicies ->
  m ExecutionResult
deleteUnreferencedFarePolicies Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  forM_ jobInfo.jobData.farePolicyIds $ \fpId -> do
    stillReferenced <- SQF.findAllFareProductByFarePolicyId fpId
    if null stillReferenced
      then do
        CQFP.delete fpId
        charges <- QCC.findAllByFp fpId.getId
        forM_ charges $ \charge -> QCC.deleteByFpAndCategory fpId.getId charge.chargeCategory
        logInfo $ "Deleted unreferenced fare policy: " <> fpId.getId
      else logInfo $ "Fare policy still referenced, not deleting: " <> fpId.getId
  return Complete
