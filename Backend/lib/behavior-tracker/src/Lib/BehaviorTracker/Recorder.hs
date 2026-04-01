{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorTracker.Recorder
  ( recordAndSnapshot,
    recordAction,
    incrementCounterOnly,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.BehaviorTracker.Accumulator
import Lib.BehaviorTracker.Types

-- | Full pipeline: increment counters + build unified snapshot
--
-- 1. Increments SWC counters per CounterConfig.counters
-- 2. Queries counters for each period in CounterConfig.periods
-- 3. Assembles BehaviorSnapshot merging: counters + event context + entity state
recordAndSnapshot ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  CounterConfig ->
  ActionEvent ->
  Value -> -- entityState: caller-provided entity data (cancellationDues, completedRides, etc.)
  m BehaviorSnapshot
recordAndSnapshot config event entityState = do
  -- Step 1: Increment counters
  incrementCounters config event

  -- Step 2: Build counter snapshot for each period
  counterMap <- buildCounterMap config event

  -- Step 3: Assemble unified snapshot
  now <- getCurrentTime
  return $
    BehaviorSnapshot
      { entityType = event.entityType,
        entityId = event.entityId,
        actionType = event.actionType,
        merchantOperatingCityId = event.merchantOperatingCityId,
        counters = counterMap,
        cooldowns = Map.empty,
        flowContext = event.flowContext,
        eventData = event.eventData,
        entityState = entityState,
        snapshotAt = now
      }

-- | Record only: increment counters without building a snapshot
--
-- Use when you just need to track an event (e.g. incrementing eligible count
-- on ride assignment) without evaluating behavior.
recordAction ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  CounterConfig ->
  ActionEvent ->
  m ()
recordAction config event = incrementCounters config event

-- | Increment a specific counter independently
--
-- Use when the eligible counter and action counter are driven by different events.
-- Example:
--   On ride assignment:  incrementCounterOnly config DRIVER driverId "RIDE_CANCELLATION" ELIGIBLE_COUNT
--   On cancellation:     recordAndSnapshot config cancelEvent entityState
incrementCounterOnly ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  CounterConfig ->
  EntityType ->
  Text -> -- entityId
  Text -> -- actionType
  CounterType ->
  m ()
incrementCounterOnly config entityType entityId actionType counterType =
  incrementCounter entityType actionType counterType entityId config.windowSizeDays

-- Internal helpers

-- | Increment all configured counters for an action event
incrementCounters ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  CounterConfig ->
  ActionEvent ->
  m ()
incrementCounters config event =
  forM_ config.counters $ \counterType ->
    incrementCounter
      event.entityType
      event.actionType
      counterType
      event.entityId
      config.windowSizeDays

-- | Build the counter map for all configured periods
buildCounterMap ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  CounterConfig ->
  ActionEvent ->
  m (Map.Map Text CounterValues)
buildCounterMap config event = do
  pairs <- forM config.periods $ \period -> do
    values <-
      buildCounterValues
        event.entityType
        event.actionType
        event.entityId
        period.periodDays
        config.windowSizeDays
    return (period.periodName, values)
  return $ Map.fromList pairs
