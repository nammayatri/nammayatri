{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Gated campaign behaviour Redis bumps (IJCampaign tag + behaviour milestones required).
-- controlFeatures-only memberships keep the tag for AA/pooling/cancel but skip ij:bm / Kafka.
-- City bt: counters are unchanged; this only writes ij:bm:… keys.
module SharedLogic.IncentiveJourney.CampaignBehaviorBump
  ( bumpIfCampaignMember,
    bumpIfCampaignMemberDetailed,
    bumpIfCampaignMemberDetailedFromMemberships,
    bumpCampaignMetricForPerson,
    bumpOnlineMinutesForClosedSession,
    OnlineMinuteCredit (..),
    membershipsFromPersonTags,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.BehaviorMetricRedis as BMRedis
import qualified Lib.IncentiveJourney.CampaignTag as CampaignTag
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.Evaluation as Evaluation
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import qualified Lib.IncentiveJourney.Window as Window
import qualified Lib.Yudhishthira.Tools.Utils as YUtils
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.IncentiveJourney.CampaignControlsCache (getCampaignControls, mkBehaviorPeriodKey)
import Storage.Beam.IncentiveJourney ()
import qualified Storage.Queries.Person as QPerson

-- | One Redis/stats credit after splitting a closed session by period day + peak window.
data OnlineMinuteCredit = OnlineMinuteCredit
  { cjmId :: Text,
    -- | Local-time sample inside the credited slice (for periodKey + inside/outside).
    sampleLocalTime :: UTCTime,
    minutes :: Int
  }
  deriving (Eq, Show, Generic)

membershipsFromPersonTags :: [LYT.TagNameValueExpiry] -> [CampaignTag.CampaignMembership]
membershipsFromPersonTags =
  concatMap
    ( \tag ->
        case T.splitOn "#" (LYT.getTagNameValueExpiry tag) of
          (name : value : _)
            | CampaignTag.isIjCampaignTagName name ->
              CampaignTag.decodeMembershipsFromTag name value
          _ -> []
    )

-- | Load person tags; if IJCampaign present and trackBehaviorMetrics, eligible for tick.
-- Redis INCR only for in-window timeBound keys; outside peak still returns True so
-- StatsSync can update *OutsideTimebound. Caller may publish BehaviourMetricTick.
bumpIfCampaignMember ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Hedis.HedisFlow m r
  ) =>
  Id Person.Person ->
  BMRedis.CampaignBehaviorMetric ->
  Int ->
  UTCTime ->
  m Bool
bumpIfCampaignMember driverId metric delta localTime =
  not . null <$> bumpIfCampaignMemberDetailed driverId metric delta localTime

-- | Same as bumpIfCampaignMember; returns eligible cjmIds (Redis may be skipped outside peak).
bumpIfCampaignMemberDetailed ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Hedis.HedisFlow m r
  ) =>
  Id Person.Person ->
  BMRedis.CampaignBehaviorMetric ->
  Int ->
  UTCTime ->
  m [Text]
bumpIfCampaignMemberDetailed driverId metric delta localTime = do
  mbPerson <- QPerson.findById driverId
  case mbPerson >>= (.driverTag) of
    Nothing -> pure []
    Just tags -> do
      now <- getCurrentTime
      bumpIfCampaignMemberDetailedFromMemberships
        driverId
        (membershipsFromPersonTags (YUtils.filterExpiredTags' now tags))
        metric
        delta
        localTime

-- | Hot-path variant: caller already has memberships (e.g. from DriverPoolResult.driverTags).
-- Skips Person KV/DB read. Empty memberships → no-op.
-- Returns eligible cjmIds for StatsSync ticks; Redis INCR still in-window only.
bumpIfCampaignMemberDetailedFromMemberships ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Hedis.HedisFlow m r
  ) =>
  Id Person.Person ->
  [CampaignTag.CampaignMembership] ->
  BMRedis.CampaignBehaviorMetric ->
  Int ->
  UTCTime ->
  m [Text]
bumpIfCampaignMemberDetailedFromMemberships _driverId [] _metric _delta _localTime = pure []
bumpIfCampaignMemberDetailedFromMemberships driverId memberships metric delta localTime = do
  bumped <- forM memberships $ \m -> do
    ok <- bumpCampaignMetricForMembership (cast driverId) m metric delta localTime
    pure $ if ok then Just m.cjmId else Nothing
  pure $ catMaybes bumped

-- | Credit OnlineMinutes for a closed session by splitting [start,end] across local days
-- and peak windows, then bumping each slice with its own sample time. Avoids:
--   * end-outside-window → zero credit for an otherwise in-window session
--   * cross-midnight session → all minutes on the end day/period
-- Inside slices also INCR Redis tb keys; outside slices still emit credits/ticks so
-- StatsSync can fill onlineMinutesOutsideTimebound (Redis tb stays in-window only).
bumpOnlineMinutesForClosedSession ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Hedis.HedisFlow m r
  ) =>
  Id Person.Person ->
  UTCTime ->
  UTCTime ->
  Seconds ->
  m [OnlineMinuteCredit]
bumpOnlineMinutesForClosedSession driverId sessionStartUtc sessionEndUtc timeDiffFromUtc = do
  let toLocal = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc)
      startLocal = toLocal sessionStartUtc
      endLocal = toLocal sessionEndUtc
  mbPerson <- QPerson.findById driverId
  case mbPerson >>= (.driverTag) of
    Nothing -> pure []
    Just tags -> do
      now <- getCurrentTime
      let memberships = membershipsFromPersonTags (YUtils.filterExpiredTags' now tags)
      creditsNested <- forM memberships $ \m -> do
        mbControls <- getCampaignControls (Id m.cjmId)
        case mbControls of
          Just controls
            | controls.enabled && controls.trackBehaviorMetrics -> do
              let segments = Window.onlineMinuteCreditSegments controls.timeBounds startLocal endLocal
              forM segments $ \(sampleLocal, mins) -> do
                -- Redis write is best-effort (only succeeds in-window for peak campaigns).
                void $ bumpCampaignMetricForMembership (cast driverId) m BMRedis.OnlineMinutes mins sampleLocal
                pure $ OnlineMinuteCredit {cjmId = m.cjmId, sampleLocalTime = sampleLocal, minutes = mins}
          _ -> pure []
      pure $ concat creditsNested

bumpCampaignMetricForPerson ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Hedis.HedisFlow m r
  ) =>
  Id IJCommon.Person ->
  BMRedis.CampaignBehaviorMetric ->
  Int ->
  UTCTime ->
  m Bool
bumpCampaignMetricForPerson personId =
  bumpIfCampaignMember (cast personId)

-- | Eligible behaviour-tracking membership -> True (caller may publish a tick).
-- Redis tb INCR only for in-window keys; outside peak still returns True so
-- StatsSync can write *OutsideTimebound into currentMetrics (same idea as
-- closed-session online credits). Non-tracking / disabled -> False.
bumpCampaignMetricForMembership ::
  ( BeamFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Hedis.HedisFlow m r
  ) =>
  Id IJCommon.Person ->
  CampaignTag.CampaignMembership ->
  BMRedis.CampaignBehaviorMetric ->
  Int ->
  UTCTime ->
  m Bool
bumpCampaignMetricForMembership personId membership metric delta localTime = do
  mbControls <- getCampaignControls (Id membership.cjmId)
  case mbControls of
    Just controls
      | controls.enabled && controls.trackBehaviorMetrics -> do
        let periodKey = mkBehaviorPeriodKey controls localTime
            keysToBump = filter (shouldBumpKey localTime controls.timeBounds) membership.timeBoundKeys
            cjmId = Id membership.cjmId :: Id DCJM.CohortJourneyMapping
            seedAction = behaviorSeedFromStats personId cjmId periodKey metric
        forM_ keysToBump $ \tbKey ->
          void $
            BMRedis.incrCampaignBehaviorMetric
              metric
              personId
              cjmId
              periodKey
              tbKey
              delta
              seedAction
        pure True
    _ -> pure False

-- | Max Inside/Outside count across period stats rows for this metric (0 if none).
behaviorSeedFromStats ::
  BeamFlow m r =>
  Id IJCommon.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  BMRedis.CampaignBehaviorMetric ->
  m Int
behaviorSeedFromStats personId cjmId periodKey metric = do
  rows <- QStats.findStatsByPersonCjmAndPeriod personId cjmId periodKey
  let base = metricBaseName metric
      seeds =
        map
          (\row -> Evaluation.lookupMetricInt row.currentMetrics mempty base)
          rows
  pure $ maximum (0 : seeds)

metricBaseName :: BMRedis.CampaignBehaviorMetric -> Text
metricBaseName = \case
  BMRedis.OnlineMinutes -> "onlineMinutes"
  BMRedis.RrReceived -> "rrReceived"
  BMRedis.RrAccepted -> "rrAccepted"
  BMRedis.DxCancel -> "dxCancel"
  BMRedis.DafCancel -> "dafCancel"

-- | Unbounded / missing → only "daily". Bounded → short "tb" (or legacy tb:… fingerprint) while inside window.
-- Window schedule itself comes from controls.timeBounds (cache), never parsed from the key string.
shouldBumpKey :: UTCTime -> Maybe TB.TimeBound -> Text -> Bool
shouldBumpKey localTime mbTb tbKey =
  case mbTb of
    Nothing -> tbKey == CampaignTag.timeBoundKeyDaily
    Just TB.Unbounded -> tbKey == CampaignTag.timeBoundKeyDaily
    Just _ ->
      Window.isWithinTimeBound localTime mbTb
        && ( tbKey == CampaignTag.timeBoundKeyBounded
               || T.isPrefixOf "tb:" tbKey
           )
