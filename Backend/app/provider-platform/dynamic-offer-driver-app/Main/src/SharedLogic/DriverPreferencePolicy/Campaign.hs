{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | A campaign's (MG) PreferencePolicy perk with @enableIn: TimeBound@ is only enforced inside the first milestone's
-- timeBounds, so a member may change a locked preference outside the window and would keep it inside. The
-- 'ApplyPreferencePolicy' job ('CampaignPolicySource') re-applies the campaign's locked values a little after every
-- window opens. This module is the campaign side of that: reading the campaign fresh, the stop conditions, the window
-- arithmetic, and queueing the chain when the campaign is saved.
--
-- One chain per campaign: every chain carries the mapping's updatedAt from when it was queued ('sourceVersion'). Each
-- save (create / update / roll, all through the dashboard handlers) moves updatedAt and queues a new chain, and a run
-- whose version is no longer the mapping's stops ('campaignWindowPolicy'). So two edits leave one live chain, and an
-- edit that turns the perk off (AllDay, removed, no values, disabled) stops the old chain at its next run.
module SharedLogic.DriverPreferencePolicy.Campaign
  ( CampaignPolicyState (..),
    readCampaignPolicyState,
    campaignWindowPolicy,
    campaignWindowOpenNow,
    nextWindowRunAt,
    nextCampaignWindowRun,
    windowStartDelay,
    scheduleCampaignPolicyChain,
  )
where

import qualified Domain.Types.DriverPreferencePolicy as DDPP
import Domain.Types.TransporterConfig (TransporterConfig (..))
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.Common
import "config-pilot" Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMappingExtra as QCJMExtra
import qualified Lib.IncentiveJourney.Streak as Streak
import qualified Lib.IncentiveJourney.Window as Window
import Lib.Scheduler.Environment (JobCreator)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), ApplyPreferencePolicyJobData (..), PolicySource (..))
import qualified SharedLogic.DriverPreferencePolicy.Apply as Apply
import SharedLogic.IncentiveJourney.CampaignControlsCache (CampaignControls (..), loadCampaignControlsFresh)
import Storage.Beam.IncentiveJourney ()
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Storage.Queries.Transformers.FleetConfig (decodePreferencePolicy)

-- | The campaign as the job and the save hook see it: the mapping and its controls read fresh (not the 60 s cache).
-- Campaign dates and timeBounds are city-local wall clock (as in the incentive-journey library).
data CampaignPolicyState = CampaignPolicyState
  { mapping :: DCJM.CohortJourneyMapping,
    controls :: CampaignControls,
    timeDiffFromUtc :: Seconds,
    localNow :: UTCTime,
    -- | Exclusive streak end, local.
    campaignEnd :: UTCTime
  }

readCampaignPolicyState ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DCJM.CohortJourneyMapping ->
  m (Maybe CampaignPolicyState)
readCampaignPolicyState cjmId =
  QCJMExtra.findCohortJourneyMappingById cjmId >>= \case
    Nothing -> pure Nothing
    Just mapping ->
      loadCampaignControlsFresh cjmId >>= \case
        Nothing -> pure Nothing
        Just controls -> do
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mapping.merchantOperatingCityId.getId}) Nothing
          let timeDiff = maybe 0 (.timeDiffFromUtc) transporterConfig
          localNow <- getLocalCurrentTime timeDiff
          pure $
            Just
              CampaignPolicyState
                { mapping,
                  controls,
                  timeDiffFromUtc = timeDiff,
                  localNow,
                  campaignEnd = Streak.computeStreakEndDate mapping.startDate mapping.streakRange controls.journeyType
                }

-- | The policy a window chain re-applies, or why the chain stops: the campaign is gone from under it (saved again
-- since the chain was queued: @Just version@ /= updatedAt), disabled, ended, its PreferencePolicy perk is removed, does
-- not decode or has no locked values, its enableIn is not TimeBound, or its first milestone has no bounded timeBounds
-- (a TimeBound perk without windows is always open: nothing opens). @Nothing@ version: no version check (the save hook).
campaignWindowPolicy :: Maybe UTCTime -> CampaignPolicyState -> Either Text DDPP.PreferencePolicy
campaignWindowPolicy mbVersion st
  | maybe False (not . sameVersion st.mapping.updatedAt) mbVersion = Left "was saved again since this chain was queued (a newer chain owns it)"
  | not st.mapping.enabled = Left "is disabled"
  | st.localNow >= st.campaignEnd = Left "has ended"
  | st.controls.preferencePolicyEnableIn /= Mg.TimeBound = Left "has no TimeBound PreferencePolicy perk (AllDay or none: no window to re-apply at)"
  | not (isBounded st.controls.timeBounds) = Left "has no bounded first-milestone timeBounds (the perk is always open)"
  | otherwise = case st.controls.preferencePolicy of
    Nothing -> Left "has no PreferencePolicy perk"
    Just raw -> case decodePreferencePolicy raw of
      Nothing -> Left "has a PreferencePolicy perk that does not decode"
      Just policy
        | Apply.policyHasValues policy -> Right policy
        | otherwise -> Left "has a PreferencePolicy perk without locked values"
  where
    isBounded = \case
      Just TB.Unbounded -> False
      Just _ -> True
      Nothing -> False

-- | updatedAt round-trips through KV (full precision) and Postgres (microseconds): equal to the millisecond.
sameVersion :: UTCTime -> UTCTime -> Bool
sameVersion a b = abs (diffUTCTime a b) < 0.001

-- | The campaign is live and its first milestone window is open now.
campaignWindowOpenNow :: CampaignPolicyState -> Bool
campaignWindowOpenNow st =
  st.mapping.startDate <= st.localNow
    && st.localNow < st.campaignEnd
    && Window.isWithinTimeBound st.localNow st.controls.timeBounds

-- | How long after a window opens the run is queued: the window check is exclusive at the start.
windowStartDelay :: NominalDiffTime
windowStartDelay = 30

-- | When to run after @nowUtc@ for windows @timeBounds@ in a campaign @[campaignStart, campaignEnd)@ (both city local):
-- the next window start inside the campaign ('Window.nextPeakStart', city local), in UTC, plus 'windowStartDelay'.
-- A window opening exactly at the campaign start counts. Nothing when no window opens before the campaign ends.
nextWindowRunAt :: Seconds -> UTCTime -> UTCTime -> TB.TimeBound -> UTCTime -> Maybe UTCTime
nextWindowRunAt timeDiffFromUtc campaignStart campaignEnd timeBounds nowUtc = do
  let offset = secondsToNominalDiffTime timeDiffFromUtc
      localNow = addUTCTime offset nowUtc
      searchFrom = max localNow (addUTCTime (-1) campaignStart)
  windowStart <- Window.nextPeakStart searchFrom timeBounds
  guard (windowStart < campaignEnd)
  pure $ addUTCTime windowStartDelay (addUTCTime (negate offset) windowStart)

nextCampaignWindowRun :: CampaignPolicyState -> Maybe UTCTime
nextCampaignWindowRun st = do
  timeBounds <- st.controls.timeBounds
  let nowUtc = addUTCTime (negate (secondsToNominalDiffTime st.timeDiffFromUtc)) st.localNow
  nextWindowRunAt st.timeDiffFromUtc st.mapping.startDate st.campaignEnd timeBounds nowUtc

-- | Campaign create / update / roll (dashboard, after the save): when the campaign has a TimeBound PreferencePolicy
-- perk with values and bounded windows and is enabled, queues its window chain: a run now when a window is open (the
-- saved values apply at once), else at the next window start. The new chain carries the mapping's updatedAt, which
-- stops any older chain of this campaign at its next run.
scheduleCampaignPolicyChain ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r, JobCreator r m) =>
  Id DCJM.CohortJourneyMapping ->
  m ()
scheduleCampaignPolicyChain cjmId = do
  let logPrefix = "Preference policy window re-apply for campaign " <> cjmId.getId
  readCampaignPolicyState cjmId >>= \case
    Nothing -> logInfo $ logPrefix <> ": campaign not found, not queued"
    Just st -> case campaignWindowPolicy Nothing st of
      Left reason -> logInfo $ logPrefix <> " not queued: the campaign " <> reason
      Right _ -> do
        let jobData =
              ApplyPreferencePolicyJobData
                { source = CampaignPolicySource cjmId.getId,
                  merchantId = cast st.mapping.merchantId,
                  merchantOperatingCityId = cast st.mapping.merchantOperatingCityId,
                  afterDriverId = Nothing,
                  attemptCount = Nothing,
                  rescheduleAtNextWindow = Just True,
                  sourceVersion = Just st.mapping.updatedAt
                }
            queueIn delay = JC.createJobIn @_ @'ApplyPreferencePolicy (Just jobData.merchantId) (Just jobData.merchantOperatingCityId) delay jobData
        if campaignWindowOpenNow st
          then do
            logInfo $ logPrefix <> ": a window is open now, queueing ApplyPreferencePolicy now"
            queueIn 0
          else case nextCampaignWindowRun st of
            Nothing -> logInfo $ logPrefix <> " not queued: no window opens before the campaign ends"
            Just runAt -> do
              now <- getCurrentTime
              logInfo $ logPrefix <> ": queueing ApplyPreferencePolicy at " <> show runAt <> " (UTC, next window start + margin)"
              queueIn (max 0 (diffUTCTime runAt now))
