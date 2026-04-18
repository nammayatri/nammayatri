{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorTracker.BlockTracker
  ( mkBlockKey,
    mkCooldownKey,
    writeBlockKey,
    writeCooldownKey,
    writeBlockAndCooldownKeys,
    readBlockKey,
    readCooldownKey,
    isEntityInCooldown,
    isEntityBlocked,
  )
where

import qualified Data.Aeson as A
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.BehaviorTracker.Types

-- | Redis key for block tracking
-- Format: "bt:block:{entityType}:{entityId}:{blockType}:{reasonTag}"
mkBlockKey :: EntityType -> Text -> BlockType -> Text -> Text
mkBlockKey entityType entityId blockType reasonTag =
  "bt:block:" <> show entityType <> ":" <> entityId <> ":" <> show blockType <> ":" <> reasonTag

-- | Redis key for cooldown tracking
-- Format: "bt:cooldown:{entityType}:{entityId}:{reasonTag}"
mkCooldownKey :: EntityType -> Text -> Text -> Text
mkCooldownKey entityType entityId reasonTag =
  "bt:cooldown:" <> show entityType <> ":" <> entityId <> ":" <> reasonTag

-- | Write a block key with TTL
writeBlockKey ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  BlockType ->
  Text -> -- reasonTag
  Int -> -- blockDurationHours (0 = permanent, uses 365 days TTL)
  Text -> -- blockReason
  A.Value -> -- extraParams
  m ()
writeBlockKey entityType entityId blockType reasonTag durationHours blockReason extraParams = do
  now <- getCurrentTime
  let key = mkBlockKey entityType entityId blockType reasonTag
      ttlSeconds = if durationHours == 0 then 365 * 24 * 3600 else durationHours * 3600
      expiresAt' = if durationHours == 0 then Nothing else Just $ addUTCTime (fromIntegral ttlSeconds) now
      info =
        BlockInfo
          { blockType = blockType,
            blockReason = blockReason,
            reasonTag = reasonTag,
            blockedAt = now,
            expiresAt = expiresAt',
            extraParams = extraParams
          }
  Redis.runInMultiCloudRedisWrite $ Redis.withCrossAppRedis $ Redis.setExp key info ttlSeconds

-- | Write a cooldown key with TTL
writeCooldownKey ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  Text -> -- reasonTag
  Int -> -- cooldownHours
  m ()
writeCooldownKey entityType entityId reasonTag cooldownHours = do
  now <- getCurrentTime
  let key = mkCooldownKey entityType entityId reasonTag
      ttlSeconds = cooldownHours * 3600
      info =
        CooldownInfo
          { reasonTag = reasonTag,
            setAt = now,
            expiresAt = addUTCTime (fromIntegral ttlSeconds) now
          }
  Redis.runInMultiCloudRedisWrite $ Redis.withCrossAppRedis $ Redis.setExp key info ttlSeconds

-- | Write both block + cooldown keys in one call
writeBlockAndCooldownKeys ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  BlockType ->
  Text -> -- reasonTag
  Int -> -- blockDurationHours
  Text -> -- blockReason
  A.Value -> -- extraParams
  Maybe Int -> -- cooldownHours (Nothing = no cooldown)
  m ()
writeBlockAndCooldownKeys entityType entityId blockType reasonTag durationHours blockReason extraParams mbCooldownHours = do
  writeBlockKey entityType entityId blockType reasonTag durationHours blockReason extraParams
  whenJust mbCooldownHours $ \cooldownHours ->
    writeCooldownKey entityType entityId reasonTag cooldownHours

-- | Read a block key: returns (BlockInfo, remainingTTL in seconds)
readBlockKey ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  BlockType ->
  Text -> -- reasonTag
  m (Maybe (BlockInfo, Integer))
readBlockKey entityType entityId blockType reasonTag = do
  let key = mkBlockKey entityType entityId blockType reasonTag
  Redis.runInMultiCloudRedisMaybeResult $
    Redis.withCrossAppRedis $ do
      mbVal <- Redis.get @BlockInfo key
      case mbVal of
        Nothing -> return Nothing
        Just info -> do
          remainingTtl <- Redis.ttl key
          -- TTL -2 = key doesn't exist, -1 = no expiry (permanent), 0 = about to expire
          if remainingTtl == -1
            then return $ Just (info, 0) -- permanent block, no expiry
            else
              if remainingTtl <= 0
                then return Nothing -- expired or non-existent
                else return $ Just (info, remainingTtl)

-- | Read a cooldown key: returns (CooldownInfo, remainingTTL in seconds)
readCooldownKey ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text -> -- entityId
  Text -> -- reasonTag
  m (Maybe (CooldownInfo, Integer))
readCooldownKey entityType entityId reasonTag = do
  let key = mkCooldownKey entityType entityId reasonTag
  Redis.runInMultiCloudRedisMaybeResult $
    Redis.withCrossAppRedis $ do
      mbVal <- Redis.get @CooldownInfo key
      case mbVal of
        Nothing -> return Nothing
        Just info -> do
          remainingTtl <- Redis.ttl key
          -- Cooldowns always have TTL; treat <= 0 as expired
          if remainingTtl <= 0
            then return Nothing
            else return $ Just (info, remainingTtl)

-- | Check if entity has an active block of given type/reason
isEntityBlocked ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text ->
  BlockType ->
  Text ->
  m Bool
isEntityBlocked entityType entityId blockType reasonTag =
  isJust <$> readBlockKey entityType entityId blockType reasonTag

-- | Check if entity is in cooldown for given reason
isEntityInCooldown ::
  ( Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  EntityType ->
  Text ->
  Text ->
  m Bool
isEntityInCooldown entityType entityId reasonTag =
  isJust <$> readCooldownKey entityType entityId reasonTag
