{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.ReconcileRewardInflight
  ( reconcileRewardInflight,
    reconcileRewardInflightImpl,
  )
where

import qualified Domain.Action.Rewards.Coupon as Coupon
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.RewardCampaignExtra as QRCmpE
import qualified Storage.Queries.RewardCohort as QRC

reconcileIntervalSeconds :: NominalDiffTime
reconcileIntervalSeconds = intToNominalDiffTime 300

reconcileRewardInflight ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'ReconcileRewardInflight ->
  m ExecutionResult
reconcileRewardInflight Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      merchantId' = jobData.merchantId
      merchantOperatingCityId' = jobData.merchantOperatingCityId
  void reconcileRewardInflightImpl
  createJobIn @_ @'ReconcileRewardInflight (Just merchantId') (Just merchantOperatingCityId') reconcileIntervalSeconds jobData
  logInfo "Scheduled next ReconcileRewardInflight job to run in 300 seconds"
  pure Complete

reconcileRewardInflightImpl ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  m ()
reconcileRewardInflightImpl = do
  activeCampaigns <- QRCmpE.findAllActive
  forM_ activeCampaigns $ \campaign ->
    QRC.findAllByCampaign campaign.id >>= \cohorts ->
      forM_ cohorts $
        void . try @_ @SomeException . Coupon.reconcileInflight campaign
