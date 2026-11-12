{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Behaviour-tick path: sync currentMetrics only (no condition eval, no reward).
module Lib.IncentiveJourney.Domain.Action.StatsSync
  ( syncBehaviorMetricToStats,
    eventNameToMetricBase,
    withStatsRowLock,
  )
where

import Control.Applicative ((<|>))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Convert as Convert
import qualified Lib.IncentiveJourney.Deferred as Deferred
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Evaluation as Evaluation
import qualified Lib.IncentiveJourney.Period as Period
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.CachedQueries.IncentiveJourneyStats as CQStats
import qualified Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyStatsExtra as QStats
import Lib.IncentiveJourney.Types.Actor (JourneyActor)
import qualified Lib.IncentiveJourney.Window as Window

-- | Serialize RMW of one stats row's currentMetrics (tick / ride-end / settle writers).
-- Kernel helper: SETNX + TTL, unlock in 'finally' (covers waive InvalidRequest), same
-- pattern as the rest of the codebase (no ownership token).
-- MonadMask comes from Kernel.Prelude (Control.Exception.Safe).
withStatsRowLock ::
  (Hedis.HedisFlow m r, MonadMask m) =>
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Id DIJM.IncentiveJourneyMilestone ->
  Text ->
  m a ->
  m a
withStatsRowLock personId cjmId milestoneId periodKey action = do
  let lockKey =
        Deferred.statsRowLockKey personId.getId cjmId.getId milestoneId.getId periodKey
  Hedis.withWaitAndLockRedis
    lockKey
    Deferred.statsRowLockTTLSeconds
    Deferred.statsRowLockRetryDelayMicros
    action

-- | Map tick actionType / metric name to currentMetrics base key.
eventNameToMetricBase :: Text -> Maybe Text
eventNameToMetricBase = \case
  "onlineMinutes" -> Just "onlineMinutes"
  "ONLINE_MINUTES" -> Just "onlineMinutes"
  "rrReceived" -> Just "rrReceived"
  "rrAccepted" -> Just "rrAccepted"
  "dxCancel" -> Just "dxCancel"
  "dafCancel" -> Just "dafCancel"
  "QUOTE_RESPONSE_ACCEPT" -> Just "rrAccepted"
  "RIDE_CANCELLATION" -> Just "dxCancel"
  _ -> Nothing

syncBehaviorMetricToStats ::
  (BeamFlow m r) =>
  JourneyActor ->
  Id Common.Person ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  DIJ.IncentiveJourney ->
  DCJM.CohortJourneyMapping ->
  [DIJM.IncentiveJourneyMilestone] ->
  UTCTime -> -- localTime
  Text -> -- metric base name
  Int -> -- delta
  m ()
syncBehaviorMetricToStats actor personId merchantId merchantOpCityId journey cjm milestones localTime metricBase delta = do
  let periodKey =
        Period.mkJourneyPeriodKeyWithStart
          localTime
          (Convert.toLibPeriodType journey.journeyType)
          cjm.startDate
      -- Not only evaluation JsonLogic: OnlineMinutes / rates / NetOff also need progressive currentMetrics.
      syncMilestones = filter needsBehaviorStatsSync milestones
  forM_ syncMilestones $ \milestone ->
    withStatsRowLock personId cjm.id milestone.id periodKey $ do
      mbExisting <-
        QStats.findStatsByPersonCjmAndMilestonePeriod personId cjm.id milestone.id periodKey
      let inside = Window.isWithinTimeBound localTime milestone.timeBounds
          newMetrics =
            Evaluation.bumpMetricCountInCurrentMetrics
              (mbExisting >>= (.currentMetrics))
              metricBase
              inside
              delta
      now <- getCurrentTime
      statsId <- maybe generateGUID (pure . (.id)) mbExisting
      void $
        CQStats.upsertJourneyStats
          actor
          DIJS.IncentiveJourneyStats
            { id = statsId,
              personId = personId,
              merchantId = merchantId,
              merchantOperatingCityId = merchantOpCityId,
              cohortId = cjm.cohortId,
              campaignUserLinkType = (mbExisting >>= (.campaignUserLinkType)) <|> cjm.campaignUserLinkType,
              cohortJourneyMappingId = Just cjm.id,
              journeyId = journey.id,
              milestoneId = milestone.id,
              periodKey = periodKey,
              conditionType = milestone.conditionType,
              conditionOperator = milestone.conditionOperator,
              conditionValue = milestone.conditionValue,
              currentValue = maybe 0 (.currentValue) mbExisting,
              currentMetrics = newMetrics,
              status = maybe DIJS.InProgress (.status) mbExisting,
              rewardType = milestone.rewardType,
              rewardValue = mbExisting >>= (.rewardValue),
              createdAt = maybe now (.createdAt) mbExisting,
              updatedAt = now
            }

-- | Rows that consume campaign behaviour counters in currentMetrics.
-- evaluation bag, simple behaviour conditionType, or NetOff (MG/online slabs).
needsBehaviorStatsSync :: DIJM.IncentiveJourneyMilestone -> Bool
needsBehaviorStatsSync m =
  isJust m.evaluation || Deferred.isDeferredMilestone m
