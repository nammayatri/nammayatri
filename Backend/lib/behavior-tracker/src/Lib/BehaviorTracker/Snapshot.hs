{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorTracker.Snapshot
  ( buildSnapshot,
    buildSnapshotWithCooldowns,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.BehaviorTracker.Accumulator (buildCounterValues)
import Lib.BehaviorTracker.BlockTracker (isEntityInCooldown)
import Lib.BehaviorTracker.Types

-- | Build a snapshot from existing counters without recording a new action
-- Cooldowns field is empty (backward compatible)
buildSnapshot ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  CounterConfig ->
  ActionEvent ->
  Value -> -- entityState: caller-provided entity data
  m BehaviorSnapshot
buildSnapshot config event entityState =
  buildSnapshotWithCooldowns config event entityState []

-- | Build a snapshot with cooldown state included
-- Provide a list of reasonTags to check.
-- Rules can reference: {"var": "cooldowns.CancellationRateDaily"} -> true/false
buildSnapshotWithCooldowns ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  CounterConfig ->
  ActionEvent ->
  Value -> -- entityState
  [Text] -> -- cooldown reasonTags to check
  m BehaviorSnapshot
buildSnapshotWithCooldowns config event entityState cooldownTags = do
  counterMap <- buildCounterMapForEntity config event.entityType event.actionType event.entityId
  cooldownMap <- buildCooldownMap event.entityType event.entityId cooldownTags
  now <- getCurrentTime
  return $
    BehaviorSnapshot
      { entityType = event.entityType,
        entityId = event.entityId,
        actionType = event.actionType,
        merchantOperatingCityId = event.merchantOperatingCityId,
        counters = counterMap,
        cooldowns = cooldownMap,
        flowContext = event.flowContext,
        eventData = event.eventData,
        entityState = entityState,
        snapshotAt = now
      }

-- Internal helpers

buildCounterMapForEntity ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  CounterConfig ->
  EntityType ->
  Text -> -- actionType
  Text -> -- entityId
  m (Map.Map Text CounterValues)
buildCounterMapForEntity config entityType actionType entityId = do
  pairs <- forM config.periods $ \period -> do
    values <-
      buildCounterValues
        entityType
        actionType
        entityId
        period.periodDays
        config.windowSizeDays
    return (period.periodName, values)
  return $ Map.fromList pairs

buildCooldownMap ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  [Text] -> -- reasonTags
  m (Map.Map Text Bool)
buildCooldownMap entityType entityId tags = do
  pairs <- forM tags $ \tag -> do
    active <- isEntityInCooldown entityType entityId tag
    return (tag, active)
  return $ Map.fromList pairs
