{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StclMembershipExtra where

import qualified Domain.Types.StclMembership as Domain
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.StclMembership as Beam
import Storage.Queries.OrphanInstances.StclMembership

-- Redis keys for STCL membership application and share count tracking
stclApplicationCountKey :: Text
stclApplicationCountKey = "stcl_application_count"

stclShareEndCountKey :: Text
stclShareEndCountKey = "stcl_share_end_count"

stclMembershipLockKey :: Text
stclMembershipLockKey = "stcl_membership_lock"

-- | Find the latest submitted membership ordered by updated_at descending
findLatestSubmittedMembership ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  m (Maybe Domain.StclMembership)
findLatestSubmittedMembership =
  findAllWithOptionsKV
    [Se.And [Se.Is Beam.status $ Se.Eq Domain.SUBMITTED]]
    (Se.Desc Beam.updatedAt)
    (Just 1)
    Nothing
    <&> listToMaybe

-- | Update application count, share start count, and share end count for a membership
updateApplicationShareCounts ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  Kernel.Types.Id.Id Domain.StclMembership ->
  m ()
updateApplicationShareCounts appCount shareStart shareEnd membershipId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.applicationCount appCount,
      Se.Set Beam.shareStartCount shareStart,
      Se.Set Beam.shareEndCount shareEnd,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId membershipId)]

-- | Atomically increment application count and share end count using Redis INCR/INCRBY.
-- Each process gets a unique value. Redis keys are updated by the atomic ops.
atomicIncrCounts ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  Int ->
  m (Int, Int, Int)
atomicIncrCounts numberOfShares = do
  newAppCount <- fromIntegral <$> Redis.incr stclApplicationCountKey
  newShareEnd <- fromIntegral <$> Redis.incrby stclShareEndCountKey (fromIntegral numberOfShares)
  let newShareStart = newShareEnd - numberOfShares + 1
  pure (newAppCount, newShareStart, newShareEnd)

-- | Initialize Redis from DB and return counts for current membership.
-- Call only when holding the lock. After init, uses atomic INCR/INCRBY.
initFromDbAndIncr ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Int ->
  m (Int, Int, Int)
initFromDbAndIncr numberOfShares = do
  mbLatest <- findLatestSubmittedMembership
  let (baseAppCount, baseShareEnd) =
        case mbLatest of
          Just m ->
            ( fromIntegral $ fromMaybe 0 m.applicationCount,
              fromIntegral $ fromMaybe 0 m.shareEndCount
            )
          Nothing -> (0 :: Integer, 0 :: Integer)
  -- Set base values so INCR/INCRBY yield correct next values
  Redis.set stclApplicationCountKey baseAppCount
  Redis.set stclShareEndCountKey baseShareEnd
  atomicIncrCounts numberOfShares

-- | Check if both Redis count keys exist (have been initialized).
redisCountKeysExist ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  m Bool
redisCountKeysExist = do
  mbApp <- Redis.get @Integer stclApplicationCountKey
  mbShare <- Redis.get @Integer stclShareEndCountKey
  pure $ isJust mbApp && isJust mbShare

-- | Update application and share counts for a new SUBMITTED membership.
-- Uses atomic Redis INCR for applicationCount and INCRBY for shareEndCount so each process gets a unique value.
-- When Redis has values: atomic ops only (no lock). When Redis empty: blocking lock; second waits; after lock,
-- first has populated Redis, so use atomic INCR/INCRBY (or init from DB if still empty).
updateApplicationAndShareCounts ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Kernel.Types.Id.Id Domain.StclMembership ->
  Int ->
  m ()
updateApplicationAndShareCounts membershipId numberOfShares = do
  exists <- redisCountKeysExist
  (newAppCount, newShareStart, newShareEnd) <-
    if exists
      then atomicIncrCounts numberOfShares
      else Redis.withLockRedisAndReturnValue stclMembershipLockKey 60 $ do
        -- Double-check Redis after acquiring lock (first process may have populated while we waited)
        exists' <- redisCountKeysExist
        if exists' then atomicIncrCounts numberOfShares else initFromDbAndIncr numberOfShares

  updateApplicationShareCounts
    (Just newAppCount)
    (Just newShareStart)
    (Just newShareEnd)
    membershipId
