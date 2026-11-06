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
    buildSnapshotWithSharedEligible,
    buildSnapshotWithEvents,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.BehaviorTracker.Accumulator (buildCounterValuesWithEligible)
import Lib.BehaviorTracker.BlockTracker (isEntityInCooldown)
import qualified Lib.BehaviorTracker.EventsWindow as EventsWindow
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
buildSnapshotWithCooldowns config event entityState cooldownTags =
  buildSnapshotInternal config event entityState cooldownTags Nothing

-- | Like buildSnapshotWithCooldowns, but the rate denominator (ELIGIBLE_COUNT) is read
-- from a shared actionType instead of the event's own actionType. Use when several
-- outcome-specific action types share one total-eligibility counter, so every outcome's
-- rate reads as "share of the shared eligibility".
buildSnapshotWithSharedEligible ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  CounterConfig ->
  ActionEvent ->
  Value -> -- entityState
  [Text] -> -- cooldown reasonTags to check
  Text -> -- actionType to read ELIGIBLE_COUNT from
  m BehaviorSnapshot
buildSnapshotWithSharedEligible config event entityState cooldownTags eligibleActionType =
  buildSnapshotInternal config event entityState cooldownTags (Just eligibleActionType)

-- | Like buildSnapshotWithCooldowns, plus the counts of an events window. With Nothing it is
-- exactly buildSnapshotWithCooldowns. With Just it reads the window of the event's entity (one
-- GET) and adds the counts of its newest X events as counters.events
-- (EventsWindow.eventsCounterName). Returns the snapshot and the length of an oversized window
-- that was counted as empty (for the caller to log), or Left with the cause when the window
-- couldn't be read, so the caller can skip rules that would judge an empty count.
-- Rules can reference: {"var": "counters.events.actionCount"}
buildSnapshotWithEvents ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  CounterConfig ->
  ActionEvent ->
  Value -> -- entityState
  [Text] -> -- cooldown reasonTags to check
  Maybe EventsWindowRead -> -- the events window to count, Nothing for none
  m (Either Text (BehaviorSnapshot, Maybe Int))
buildSnapshotWithEvents config event entityState cooldownTags = \case
  Nothing -> Right . (,Nothing) <$> buildSnapshotWithCooldowns config event entityState cooldownTags
  Just window -> do
    result <- withTryCatch "behaviorTracker:eventsWindowRead" $ EventsWindow.getWindow window.readEntity window.readKeyName event.entityId
    case result of
      Left err ->
        pure $ Left $ "the events window " <> EventsWindow.windowKey window.readEntity window.readKeyName event.entityId <> " couldn't be read because the Redis read failed: " <> show err
      Right stored -> do
        snapshot <- buildSnapshotWithCooldowns config event entityState cooldownTags
        let (counts, mbOversized) = EventsWindow.countLast window.readLastX stored
        pure $ Right ((snapshot :: BehaviorSnapshot) {counters = Map.insert EventsWindow.eventsCounterName counts snapshot.counters}, mbOversized)

buildSnapshotInternal ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  CounterConfig ->
  ActionEvent ->
  Value ->
  [Text] ->
  Maybe Text -> -- ELIGIBLE_COUNT actionType override
  m BehaviorSnapshot
buildSnapshotInternal config event entityState cooldownTags mbEligibleActionType = do
  counterMap <- buildCounterMapForEntity config event.entityType event.actionType mbEligibleActionType event.entityId
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
  Maybe Text -> -- ELIGIBLE_COUNT actionType override
  Text -> -- entityId
  m (Map.Map Text CounterValues)
buildCounterMapForEntity config entityType actionType mbEligibleActionType entityId = do
  pairs <- forM config.periods $ \period -> do
    values <-
      buildCounterValuesWithEligible
        config.hashTagEntityId
        entityType
        actionType
        mbEligibleActionType
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
