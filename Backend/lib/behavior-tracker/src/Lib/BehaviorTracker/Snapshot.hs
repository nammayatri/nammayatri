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
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.BehaviorTracker.Accumulator (buildCounterValues)
import Lib.BehaviorTracker.Types

-- | Build a snapshot from existing counters without recording a new action
--
-- Used for read-only check scenarios:
--   - Pre-booking block check: "does this rider have active blocks?"
--   - Profile fetch: "what's this driver's current behavior state?"
--   - Pre-assignment filter: "should we avoid assigning to this driver?"
buildSnapshot ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  CounterConfig ->
  EntityType ->
  Text -> -- entityId
  ActionType ->
  Text -> -- merchantOperatingCityId
  Value -> -- flowContext
  Value -> -- eventData
  Value -> -- entityState
  m BehaviorSnapshot
buildSnapshot config entityType entityId actionType merchantOperatingCityId flowContext eventData entityState = do
  counterMap <- buildCounterMapForEntity config entityType actionType entityId
  now <- getCurrentTime
  return $
    BehaviorSnapshot
      { entityType = entityType,
        entityId = entityId,
        actionType = actionType,
        merchantOperatingCityId = merchantOperatingCityId,
        counters = counterMap,
        flowContext = flowContext,
        eventData = eventData,
        entityState = entityState,
        snapshotAt = now
      }

-- Internal helper

buildCounterMapForEntity ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  CounterConfig ->
  EntityType ->
  ActionType ->
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
