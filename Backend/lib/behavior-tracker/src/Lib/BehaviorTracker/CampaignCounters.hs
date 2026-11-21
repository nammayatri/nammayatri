{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Campaign-scoped behaviour counters (Incentive Journey).
-- Lives next to city 'bt:' SWC counters ('Lib.BehaviorTracker.Accumulator') so all
-- behaviour Redis ownership stays in this package — different keyspace on purpose.
--
-- Key: ij:bm:{metric}:{driverId}:{cjmId}:{periodKey}:{timeBoundKey}
--   metric     — onlineMinutes | rrReceived | rrAccepted | dxCancel | dafCancel | …
--   periodKey  — Day:… / Week:… / Month:… (same as IJ stats)
--   timeBoundKey — daily | tb
--
-- No per-event once-keys: rrReceived counts each SRD batch send; accept/cancel rely on
-- domain gates (AlreadyAccepted / cancellable). Do NOT write these into 'bt:DRIVER:…'.
module Lib.BehaviorTracker.CampaignCounters
  ( campaignBehaviorKeyPrefix,
    mkCampaignBehaviorKey,
    campaignBehaviorTtlSeconds,
    incrCampaignBehaviorCounter,
    getCampaignBehaviorCounter,
    getCampaignBehaviorCounterSum,
  )
where

import Data.List (nub)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common

-- | Redis namespace for campaign behaviour (distinct from city 'bt:').
campaignBehaviorKeyPrefix :: Text
campaignBehaviorKeyPrefix = "ij:bm:"

-- | Per-period keys; TTL refreshed on write. 45d covers long weekly/monthly streaks.
campaignBehaviorTtlSeconds :: Hedis.ExpirationTime
campaignBehaviorTtlSeconds = 45 * 24 * 3600

mkCampaignBehaviorKey ::
  Text -> -- metric base name
  Text -> -- driver / person id
  Text -> -- cohort journey mapping id
  Text -> -- periodKey
  Text -> -- timeBoundKey (daily | tb)
  Text
mkCampaignBehaviorKey metric driverId cjmId periodKey timeBoundKey =
  campaignBehaviorKeyPrefix
    <> metric
    <> ":"
    <> driverId
    <> ":"
    <> cjmId
    <> ":"
    <> periodKey
    <> ":"
    <> timeBoundKey

-- | INCRBY delta. On key miss, run seedAction (e.g. stats lookup) and restore before incr
-- so a wiped Redis does not restart below DB. seedAction runs only on miss.
--
-- INCRBY + EXPIRE are primary-only. Wrapping INCRBY in 'runInMultiCloudRedisWrite' adds
-- +delta twice when secondaryHedisClusterEnv is another node of the same cluster (local
-- 30001/30002). Seed restore stays a dual-cloud SET (absolute value; safe if primary==secondary).
incrCampaignBehaviorCounter ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  Text -> -- metric
  Text -> -- driverId
  Text -> -- cjmId
  Text -> -- periodKey
  Text -> -- timeBoundKey
  Int ->
  m Int -> -- seedAction
  m Int
incrCampaignBehaviorCounter metric driverId cjmId periodKey timeBoundKey delta seedAction = do
  let key = mkCampaignBehaviorKey metric driverId cjmId periodKey timeBoundKey
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
  newVal <- Hedis.withCrossAppRedis $ Hedis.incrby key (fromIntegral d)
  void $ Hedis.withCrossAppRedis $ Hedis.expire key campaignBehaviorTtlSeconds
  pure $ fromIntegral newVal

getCampaignBehaviorCounter ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  m Int
getCampaignBehaviorCounter metric driverId cjmId periodKey timeBoundKey = do
  let key = mkCampaignBehaviorKey metric driverId cjmId periodKey timeBoundKey
  Hedis.withCrossAppRedis (Hedis.get key) >>= \case
    Just (n :: Integer) -> pure $ fromIntegral n
    Nothing -> pure 0

-- | Sum one metric across several timeBound keys for one period.
getCampaignBehaviorCounterSum ::
  (Hedis.HedisFlow m r, CacheFlow m r) =>
  Text ->
  Text ->
  Text ->
  Text ->
  [Text] ->
  m Int
getCampaignBehaviorCounterSum _metric _driverId _cjmId _periodKey [] = pure 0
getCampaignBehaviorCounterSum metric driverId cjmId periodKey timeBoundKeys = do
  counts <- forM (nub timeBoundKeys) $ \tbKey ->
    getCampaignBehaviorCounter metric driverId cjmId periodKey tbKey
  pure $ sum counts
