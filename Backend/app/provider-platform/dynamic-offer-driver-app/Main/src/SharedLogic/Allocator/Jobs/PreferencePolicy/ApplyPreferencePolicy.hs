{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Applies a source's preference policy to the drivers that source is in charge of, 'pageSize' per run in driver-id
--   order, then queues the next page a little later. One job type for every source ('PolicySource'): the page runner
--   ('runPage': retry, paging, apply, counts) is shared, and each source only supplies a 'PolicySourceOps'
--   (one case per source in 'applyPreferencePolicyJob'):
--
--   * 'currentPolicy': a fresh read of the source's policy (never a cache: this process didn't see the save's cache
--     invalidation). Re-read on every page, so a later save (or a clear) wins over an older run; Left stops the run.
--   * 'membersPage': the source's members after a driver-id cursor.
--   * 'stillInCharge': re-checks one member against the shared resolver at apply time (left the source since the
--     page was read, or another source is in charge: skipped).
--   * 'nextWindowRun': for a source whose policy is time-windowed, when to run the chain again (with
--     'rescheduleAtNextWindow'): a little after its next window opens; Nothing for the others.
--
--   Adding a source (e.g. a city or ops policy) = a 'PolicySource' constructor, its 'PolicySourceOps' case in
--   'applyPreferencePolicyJob', and a createJobIn where that source's policy is saved.
--
--   Sources:
--
--   * 'FleetPolicySource' (postDriverFleetConfigUpdate): fleet_config.preference_policy on the fleet's active drivers;
--     a driver a campaign is in charge of is skipped.
--   * 'CampaignPolicySource' (campaign create / update / roll, 'Campaign.scheduleCampaignPolicyChain'): a campaign's
--     TimeBound PreferencePolicy perk, re-applied to its members a little after each window opens (a member may have
--     changed a locked preference outside the window), then queued again for the next window. One live chain per
--     campaign ('sourceVersion'); see 'SharedLogic.DriverPreferencePolicy.Campaign'.
module SharedLogic.Allocator.Jobs.PreferencePolicy.ApplyPreferencePolicy
  ( applyPreferencePolicyJob,
    pageSize,
    PolicySourceOps (..),
  )
where

import qualified Domain.Types.DriverPreferencePolicy as DDPP
import qualified Domain.Types.FleetConfig as Fields
import qualified Domain.Types.FleetDriverAssociation as Fields
import Domain.Types.Person (Person)
import qualified Domain.Types.Person as Fields
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow as IJBeamFlow
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), ApplyPreferencePolicyJobData (..), PolicySource (..))
import qualified SharedLogic.DriverPreferencePolicy as DPP
import qualified SharedLogic.DriverPreferencePolicy.Apply as Apply
import qualified SharedLogic.DriverPreferencePolicy.Campaign as Campaign
import qualified SharedLogic.IncentiveJourney.CampaignControlsCache as CampaignControlsCache
import qualified SharedLogic.IncentiveJourney.MgCampaign as MgCampaign
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import Tools.Error

pageSize :: Int
pageSize = 50

-- | App-level retry of a failed page (the repo's "Pattern B", as BulkUserCohortMappingUpload): the page runs under a
-- local catch and the run always returns Complete; a failed page is queued again with the same cursor and
-- attemptCount + 1, up to 'maxAttempts'. The scheduler's jobRetryOnExceptionMap stays off for this type: on a
-- RedisBased allocator it ends in Terminate (or a broken retry), and the page chain would stop silently.
maxAttempts :: Int
maxAttempts = 5

-- | What a source supplies to the shared page runner.
data PolicySourceOps m = PolicySourceOps
  { -- | Names the source in the log lines ("fleet <id>").
    label :: Text,
    -- | Fresh read of the source's policy. Left: why the run stops (logged as "<label> <reason>, stopping").
    currentPolicy :: m (Either Text DDPP.PreferencePolicy),
    -- | Up to n members after the cursor, in driver-id order.
    membersPage :: Maybe (Id Person) -> Int -> m [Id Person],
    -- | Right driver: the source is still in charge of the member with this policy, apply. Left reason: skipped
    -- (logged, counted).
    stillInCharge :: DDPP.PreferencePolicy -> Id Person -> m (Either Text Person),
    -- | When the chain runs again after its last page, if 'rescheduleAtNextWindow' is set (UTC): a little after the
    -- source's next window opens, so the run is inside it. Nothing: the chain ends.
    nextWindowRun :: m (Maybe UTCTime)
  }

type PolicyJobFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    IJBeamFlow.BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  )

applyPreferencePolicyJob ::
  PolicyJobFlow m r =>
  Job 'ApplyPreferencePolicy ->
  m ExecutionResult
applyPreferencePolicyJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      -- The per-source interface: one case per 'PolicySource'.
      ops = case jobData.source of
        FleetPolicySource fleetOwnerId -> fleetOps fleetOwnerId
        CampaignPolicySource cjmId -> campaignOps jobData (Id cjmId)
      attempt = fromMaybe 1 jobData.attemptCount
      pageName = "ApplyPreferencePolicy: " <> ops.label <> " page after " <> maybe "start" (.getId) jobData.afterDriverId
  withTryCatch "ApplyPreferencePolicy:page" (runPage ops jobData) >>= \case
    Right () -> pure ()
    Left err
      | attempt >= maxAttempts ->
        logError $ pageName <> " failed, attempts exhausted (" <> show attempt <> "/" <> show maxAttempts <> "), the run stops here: " <> show err
      | otherwise -> do
        let delay = 5 * attempt -- backoff before attempt n + 1: 5 s, 10 s, 15 s, 20 s
        logError $ pageName <> " failed (attempt " <> show attempt <> "/" <> show maxAttempts <> "), retrying the same page in " <> show delay <> " s: " <> show err
        withTryCatch "ApplyPreferencePolicy:retry" (queueRun jobData (fromIntegral delay) jobData {attemptCount = Just (attempt + 1)})
          >>= either (\schedErr -> logError $ pageName <> ": scheduling the retry failed, the run stops here: " <> show schedErr) (const $ pure ())
  pure Complete

queueRun :: PolicyJobFlow m r => ApplyPreferencePolicyJobData -> NominalDiffTime -> ApplyPreferencePolicyJobData -> m ()
queueRun jobData delay next =
  JC.createJobIn @_ @'ApplyPreferencePolicy (Just jobData.merchantId) (Just jobData.merchantOperatingCityId) delay next

-- | One page: re-reads the policy, applies it to the next 'pageSize' members the source is still in charge of, logs the
-- counts and queues the next page (first attempt again); after the last page, the next window's run when asked. A
-- per-driver failure is counted, not thrown; anything thrown here fails the page.
runPage :: PolicyJobFlow m r => PolicySourceOps m -> ApplyPreferencePolicyJobData -> m ()
runPage ops jobData = do
  let logPrefix = "ApplyPreferencePolicy: " <> ops.label
  ops.currentPolicy >>= \case
    Left reason -> logInfo $ logPrefix <> " " <> reason <> ", stopping"
    Right policy
      | not (Apply.policyHasValues policy) -> logInfo $ logPrefix <> " has no policy values any more, stopping"
      | otherwise -> do
        driverIds <- ops.membersPage jobData.afterDriverId pageSize
        results <- forM driverIds $ \driverId ->
          withTryCatch "ApplyPreferencePolicy:driver" (applyToMember ops policy driverId) >>= \case
            Left err -> do
              logError $ "ApplyPreferencePolicy: driver " <> driverId.getId <> " failed: " <> show err
              pure Nothing
            Right outcome -> pure (Just outcome)
        let count p = length (filter p results)
            skipped = count \case Just (Apply.Skipped _) -> True; _ -> False
        logInfo $
          logPrefix <> " page after " <> maybe "start" (.getId) jobData.afterDriverId
            <> ": "
            <> show (length driverIds)
            <> " drivers, "
            <> show (count (== Just Apply.Changed))
            <> " changed, "
            <> show (count (== Just Apply.Unchanged))
            <> " unchanged, "
            <> show skipped
            <> " skipped, "
            <> show (count isNothing)
            <> " failed"
        case lastMay driverIds of
          Just lastDriverId
            | length driverIds == pageSize ->
              queueRun jobData 2 jobData {afterDriverId = Just lastDriverId, attemptCount = Nothing}
          _ -> do
            logInfo $ logPrefix <> " done"
            when (jobData.rescheduleAtNextWindow == Just True) $ scheduleNextWindow ops jobData

-- | The chain again from the first page at the source's next window ('nextWindowRun').
scheduleNextWindow :: PolicyJobFlow m r => PolicySourceOps m -> ApplyPreferencePolicyJobData -> m ()
scheduleNextWindow ops jobData =
  ops.nextWindowRun >>= \case
    Nothing -> logInfo $ "ApplyPreferencePolicy: " <> ops.label <> " has no next window, the chain ends"
    Just runAt -> do
      now <- getCurrentTime
      logInfo $ "ApplyPreferencePolicy: " <> ops.label <> " next run at " <> show runAt <> " (UTC, next window start + margin)"
      queueRun jobData (max 0 (diffUTCTime runAt now)) jobData {afterDriverId = Nothing, attemptCount = Nothing}

applyToMember :: PolicyJobFlow m r => PolicySourceOps m -> DDPP.PreferencePolicy -> Id Person -> m Apply.ApplyOutcome
applyToMember ops policy driverId =
  ops.stillInCharge policy driverId >>= \case
    Left reason -> do
      logInfo $ "ApplyPreferencePolicy: driver " <> driverId.getId <> ": " <> reason <> ", skipped"
      pure (Apply.Skipped reason)
    Right driver -> Apply.applyPreferencePolicyWithOutcome policy driver

-- | A fleet's policy (fleet_config, from the table: not the ConfigPilot cache) on its active drivers. A driver whose
-- newest active association is another fleet, or whom a campaign is in charge of, is skipped. A resolver read error
-- fails that driver (counted), never applies the fleet's policy over a campaign.
fleetOps :: PolicyJobFlow m r => Text -> PolicySourceOps m
fleetOps fleetOwnerId =
  PolicySourceOps
    { label = "fleet " <> fleetOwnerId,
      currentPolicy =
        maybe (Left "has no policy values any more") Right . (>>= (.preferencePolicy))
          <$> QFC.findByPrimaryKey (Id fleetOwnerId :: Id Person),
      membersPage = QFDA.findActiveDriverIdsByFleetOwnerIdAfter fleetOwnerId,
      stillInCharge = \_policy driverId -> do
        associations <- QFDA.findAllByDriverId driverId True
        case associations of
          (newest : _) | newest.fleetOwnerId == fleetOwnerId -> do
            driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
            context <- DPP.tryDriverFleetContext DPP.Cached driverId driver.merchantOperatingCityId >>= either throwM pure
            pure $
              if DPP.isCampaignControlled context
                then Left "a campaign is in charge"
                else Right driver
          _ -> pure $ Left ("no longer active in fleet " <> fleetOwnerId),
      nextWindowRun = pure Nothing
    }

-- | A campaign's TimeBound PreferencePolicy perk on its current members ('Campaign.campaignWindowPolicy' has the stop
-- conditions; the mapping and controls are read fresh on every page). Members: the campaign's active
-- user_cohort_mapping rows (opted in, for an OptIn campaign). A member is applied only when the resolver says this
-- campaign is in charge of them with its preference perk active now and the policy in charge is this campaign's (the
-- allocator's controls cache entry for the campaign is refreshed first, so the resolver sees the save).
campaignOps :: PolicyJobFlow m r => ApplyPreferencePolicyJobData -> Id DCJM.CohortJourneyMapping -> PolicySourceOps m
campaignOps jobData cjmId =
  PolicySourceOps
    { label = "campaign " <> cjmId.getId,
      currentPolicy = do
        CampaignControlsCache.refreshCampaignControls cjmId
        Campaign.readCampaignPolicyState cjmId <&> \case
          Nothing -> Left "not found"
          Just st -> Campaign.campaignWindowPolicy jobData.sourceVersion st,
      membersPage = \mbAfter limit ->
        Campaign.readCampaignPolicyState cjmId >>= \case
          Nothing -> pure []
          Just st -> do
            let optedInOnly = st.controls.campaignUserLinkType == Just DCJM.OptIn
            map cast <$> QUCMExtra.findActiveUserIdsByCohortMappingIdAfter cjmId st.localNow optedInOnly (cast <$> mbAfter) limit,
      stillInCharge = \policy driverId -> do
        driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
        context <- DPP.tryDriverFleetContext DPP.Cached driverId driver.merchantOperatingCityId >>= either throwM pure
        pure $
          if
              | not (DPP.isCampaignControlled context) -> Left "no campaign is in charge (membership tag missing or expired)"
              | isNothing (MgCampaign.mgPreferencePolicyValue context.mgPerkSource) -> Left "the campaign's preference perk is not active now"
              | DPP.contextPolicy context /= Just policy -> Left "another campaign's policy is in charge"
              | otherwise -> Right driver,
      nextWindowRun = (>>= Campaign.nextCampaignWindowRun) <$> Campaign.readCampaignPolicyState cjmId
    }
