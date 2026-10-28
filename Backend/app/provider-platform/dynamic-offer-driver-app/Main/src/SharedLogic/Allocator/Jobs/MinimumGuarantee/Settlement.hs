{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Settlement job of one Minimum Guarantee (MG) campaign. It reschedules itself every 15 minutes
-- (a minute when a full batch was just processed), settles every enrollment whose day's last
-- window end + grace has passed, and ends once the campaign is over (its end date has passed, or it
-- was cancelled) and nothing is left to settle. A cancelled campaign keeps its chain until the days
-- that were already over have been settled. Started when the campaign is published.
module SharedLogic.Allocator.Jobs.MinimumGuarantee.Settlement
  ( minimumGuaranteeSettlement,
  )
where

import qualified Data.Map as M
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MgCampaign as DMgC
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), MinimumGuaranteeSettlementJobData (..))
import SharedLogic.MinimumGuarantee.Context (MgEventFlow)
import SharedLogic.MinimumGuarantee.Enrollment (cancelEnrollment)
import SharedLogic.MinimumGuarantee.Rules
import SharedLogic.MinimumGuarantee.Settlement (settleEnrollment)
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.MgCampaign as QMgC
import qualified Storage.Queries.MgCampaignRule as QMgR
import qualified Storage.Queries.MgCampaignWindow as QMgW
import qualified Storage.Queries.MgEnrollmentExtra as QMgEx

type MgSettlementJobFlow m r =
  ( MgEventFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  )

settlementBatchSize :: Int
settlementBatchSize = 200

-- | Delay between runs while there is nothing backed up.
regularDelay :: NominalDiffTime
regularDelay = 15 * 60

-- | Delay after a full batch, since more rows are probably waiting.
backlogDelay :: NominalDiffTime
backlogDelay = 60

-- | Starts (or restarts) the campaign's settlement chain, first run after `delay`.
scheduleMinimumGuaranteeSettlement ::
  MgSettlementJobFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DMgC.MgCampaign ->
  NominalDiffTime ->
  m ()
scheduleMinimumGuaranteeSettlement merchantId merchantOperatingCityId campaignId delay =
  JC.createJobIn @_ @'MinimumGuaranteeSettlement (Just merchantId) (Just merchantOperatingCityId) delay $
    MinimumGuaranteeSettlementJobData {merchantId, merchantOperatingCityId, campaignId}

minimumGuaranteeSettlement ::
  MgSettlementJobFlow m r =>
  Job 'MinimumGuaranteeSettlement ->
  m ExecutionResult
minimumGuaranteeSettlement Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      campaignId = jobData.campaignId
  mbCampaign <- QMgC.findByPrimaryKey campaignId
  case mbCampaign of
    Nothing -> pure $ Terminate $ "MG settlement: campaign " <> campaignId.getId <> " not found; chain ends"
    Just campaign
      | campaign.status == DMgC.CLOSED ->
        pure $ Terminate $ "MG settlement: campaign " <> campaignId.getId <> " is closed; chain ends"
    Just campaign -> do
      -- defends against a duplicate chain firing at the same time
      result <- Redis.whenWithLockRedisAndReturnValue ("MgSettlementJob:" <> campaignId.getId) 900 $ do
        outcome <- withTryCatch "MinimumGuaranteeSettlement:runOnce" (runOnce jobData campaign)
        case outcome of
          Right r -> pure r
          Left err -> do
            -- a failed run must not end the chain: log it and try again at the next interval
            logError $ "MG settlement run failed for campaign " <> campaignId.getId <> ": " <> show err
            scheduleMinimumGuaranteeSettlement jobData.merchantId jobData.merchantOperatingCityId campaignId regularDelay
            pure Complete
      pure $ case result of
        Right r -> r
        Left _ -> Terminate $ "MG settlement: another run holds the lock for campaign " <> campaignId.getId <> "; duplicate"

runOnce :: MgSettlementJobFlow m r => MinimumGuaranteeSettlementJobData -> DMgC.MgCampaign -> m ExecutionResult
runOnce jobData campaign = do
  windows <- QMgW.findAllByCampaignId campaign.id
  rules <- QMgR.findAllByCampaignId campaign.id
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = campaign.merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound campaign.merchantOperatingCityId.getId)
  now <- getCurrentTime
  let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
  case windows of
    [] -> pure $ Terminate $ "MG settlement: campaign " <> campaign.id.getId <> " has no windows; chain ends"
    (firstWindow : otherWindows) -> do
      -- the window that ends last decides when a day is due; one pass, no sorting
      let lastWindow = foldl' (\a w -> if w.endTime > a.endTime then w else a) firstWindow otherWindows
      let dueDay = lastDueDay timeDiff campaign.settlementGraceMinutes lastWindow.endTime now
      due <- QMgEx.findAllUpcomingUpToDay campaign.id dueDay settlementBatchSize
      -- a cancelled campaign's cancel pass calls off every day from the cancellation day on. A day
      -- from then on that is still UPCOMING was missed by that pass (an interrupted run, or an opt-in
      -- that raced it): it is called off here instead of settled. The campaign row has not changed
      -- since it was cancelled, so its updatedAt is the cancellation time.
      let cancelledFromDay = localDayOf timeDiff campaign.updatedAt
          missedByCancel enrollment = campaign.status == DMgC.CANCELLED && enrollment.periodDate >= cancelledFromDay
      forM_ due $ \enrollment -> do
        res <-
          withTryCatch "MinimumGuaranteeSettlement:settleEnrollment" $
            if missedByCancel enrollment
              then void $ cancelEnrollment campaign enrollment
              else settleEnrollment campaign rules (windowSpansOnDay timeDiff enrollment.periodDate windows) enrollment
        case res of
          Left err -> logError $ "MG settlement failed for enrollment " <> enrollment.id.getId <> ": " <> show err
          Right () -> pure ()
      -- anything still UPCOMING (days that are not due yet) keeps the chain alive
      remaining <- QMgEx.findAllUpcomingUpToDay campaign.id campaign.endDate 1
      let campaignOver = campaign.status == DMgC.CANCELLED || localDayOf timeDiff now > campaign.endDate
      if campaignOver && null remaining
        then do
          -- a cancelled campaign keeps its CANCELLED status
          when (campaign.status /= DMgC.CANCELLED) $ QMgC.updateStatusById DMgC.CLOSED campaign.id
          pure Complete
        else do
          let delay = if length due >= settlementBatchSize then backlogDelay else regularDelay
          scheduleMinimumGuaranteeSettlement jobData.merchantId jobData.merchantOperatingCityId campaign.id delay
          pure Complete
