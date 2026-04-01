{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorTracker.Visibility
  ( queryEntityVisibility,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.BehaviorTracker.Accumulator (buildCounterValues)
import Lib.BehaviorTracker.BlockTracker (readBlockKey, readCooldownKey)
import Lib.BehaviorTracker.Types

-- | Query full behavior visibility for an entity.
-- Config-driven: caller provides which action types and reason tags to probe.
-- No Redis SCAN required.
queryEntityVisibility ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  BehaviorDomainConfig ->
  m EntityBehaviorVisibility
queryEntityVisibility entityType entityId config = do
  now <- getCurrentTime

  -- 1. Gather counters for all configured action types
  counterMap <- fmap Map.fromList $
    forM config.actionTypes $ \(actionType, counterCfg) -> do
      periodMap <- fmap Map.fromList $
        forM counterCfg.periods $ \period -> do
          vals <-
            buildCounterValues
              entityType
              actionType
              entityId
              period.periodDays
              counterCfg.windowSizeDays
          return (period.periodName, vals)
      return (actionType, periodMap)

  -- 2. Gather active blocks: for each (reasonTag, blockType) pair, probe Redis
  activeBlocks <- fmap concat $
    forM config.blockReasonTags $ \tag ->
      fmap catMaybes $
        forM config.blockTypes $ \bt -> do
          mbResult <- readBlockKey entityType entityId bt tag
          return $ case mbResult of
            Nothing -> Nothing
            Just (info, remainingTtl) ->
              Just $
                ActiveBlockInfo
                  { blockType = info.blockType,
                    blockReason = info.blockReason,
                    reasonTag = info.reasonTag,
                    blockedAt = info.blockedAt,
                    expiresAt = info.expiresAt,
                    remainingSeconds = remainingTtl,
                    extraParams = info.extraParams
                  }

  -- 3. Gather active cooldowns: for each reasonTag, probe Redis
  activeCooldowns <- fmap catMaybes $
    forM config.blockReasonTags $ \tag -> do
      mbResult <- readCooldownKey entityType entityId tag
      return $ case mbResult of
        Nothing -> Nothing
        Just (info, remainingTtl) ->
          Just $
            ActiveCooldownInfo
              { reasonTag = info.reasonTag,
                setAt = info.setAt,
                expiresAt = info.expiresAt,
                remainingSeconds = remainingTtl
              }

  return $
    EntityBehaviorVisibility
      { entityId = entityId,
        entityType = entityType,
        counters = counterMap,
        activeBlocks = activeBlocks,
        activeCooldowns = activeCooldowns,
        queriedAt = now
      }
