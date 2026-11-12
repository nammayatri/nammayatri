{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Campaign-scoped behaviour counters (gated; not city bt: keys).
-- Key: ij:bm:{metric}:{driverId}:{cjmId}:{periodKey}:{timeBoundKey}
-- periodKey matches stats (Day:… / Week:… / Month:…) so daily journeys do not leak across days.
module Lib.IncentiveJourney.BehaviorMetricRedis
  ( CampaignBehaviorMetric (..),
    metricToText,
    campaignBehaviorKey,
    incrCampaignBehaviorMetric,
    getCampaignBehaviorMetric,
    getCampaignBehaviorMetrics,
    campaignBehaviorTtlSeconds,
  )
where

import Data.List (nub)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common

data CampaignBehaviorMetric
  = OnlineMinutes
  | RrReceived
  | RrAccepted
  | DxCancel
  | DafCancel
  deriving (Eq, Show, Generic)

metricToText :: CampaignBehaviorMetric -> Text
metricToText = \case
  OnlineMinutes -> "onlineMinutes"
  RrReceived -> "rrReceived"
  RrAccepted -> "rrAccepted"
  DxCancel -> "dxCancel"
  DafCancel -> "dafCancel"

-- | Per-period keys; TTL refreshed on write. 45d covers long weekly/monthly streaks.
campaignBehaviorTtlSeconds :: Hedis.ExpirationTime
campaignBehaviorTtlSeconds = 45 * 24 * 3600

campaignBehaviorKey ::
  CampaignBehaviorMetric ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text -> -- periodKey (Day:… | Week:… | Month:…)
  Text -> -- timeBoundKey (daily | tb)
  Text
campaignBehaviorKey metric personId cjmId periodKey timeBoundKey =
  "ij:bm:"
    <> metricToText metric
    <> ":"
    <> personId.getId
    <> ":"
    <> cjmId.getId
    <> ":"
    <> periodKey
    <> ":"
    <> timeBoundKey

-- | INCRBY delta. If key is missing, run seedAction (typically load from stats) and restore
-- before incr so a wiped Redis does not restart below DB currentMetrics. seedAction runs
-- only on miss (no extra DB on the hot path when the key exists).
incrCampaignBehaviorMetric ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  CampaignBehaviorMetric ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Text ->
  Int ->
  m Int -> -- seedAction: stats lookup; ignored when key exists
  m Int
incrCampaignBehaviorMetric metric personId cjmId periodKey timeBoundKey delta seedAction = do
  let key = campaignBehaviorKey metric personId cjmId periodKey timeBoundKey
      d = max 0 delta
  mbExisting <- Hedis.withCrossAppRedis (Hedis.get key)
  case mbExisting of
    Nothing -> do
      seed <- max 0 <$> seedAction
      when (seed > 0) $
        void $
          Hedis.runInMultiCloudRedisWrite $
            Hedis.withCrossAppRedis $
              Hedis.setExp key (fromIntegral seed :: Integer) campaignBehaviorTtlSeconds
    Just (_ :: Integer) -> pure ()
  newVal <-
    Hedis.runInMultiCloudRedisWrite $
      Hedis.withCrossAppRedis $
        Hedis.incrby key (fromIntegral d)
  void $
    Hedis.runInMultiCloudRedisWrite $
      Hedis.withCrossAppRedis $
        Hedis.expire key campaignBehaviorTtlSeconds
  pure $ fromIntegral newVal

getCampaignBehaviorMetric ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  CampaignBehaviorMetric ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  Text ->
  m Int
getCampaignBehaviorMetric metric personId cjmId periodKey timeBoundKey = do
  let key = campaignBehaviorKey metric personId cjmId periodKey timeBoundKey
  Hedis.withCrossAppRedis (Hedis.get key) >>= \case
    Just (n :: Integer) -> pure $ fromIntegral n
    Nothing -> pure 0

-- | Sum a metric across several timeBound keys for one period.
getCampaignBehaviorMetrics ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  CampaignBehaviorMetric ->
  Id Common.Person ->
  Id DCJM.CohortJourneyMapping ->
  Text ->
  [Text] ->
  m Int
getCampaignBehaviorMetrics _metric _personId _cjmId _periodKey [] = pure 0
getCampaignBehaviorMetrics metric personId cjmId periodKey timeBoundKeys = do
  counts <- forM (nub timeBoundKeys) $ \tbKey ->
    getCampaignBehaviorMetric metric personId cjmId periodKey tbKey
  pure $ sum counts
