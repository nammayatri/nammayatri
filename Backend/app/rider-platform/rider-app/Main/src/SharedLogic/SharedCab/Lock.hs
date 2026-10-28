-- | Owner-token Redis locks. Each holder sets the key to its own random token, and the release deletes the key only while it still
-- holds that token, so a holder that outlived its TTL cannot free the lock a later holder took.
module SharedLogic.SharedCab.Lock
  ( shared,
    withOwnedLock,
    waitUntil,
    tryWithOwnedLock,
    acquireOwned,
    releaseOwned,
  )
where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Redis as RawRedis
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator (withTimeRedis)
import System.Random (randomRIO)
import Tools.Error (RedisLockError (..))

-- | The cross-app master cell, unprefixed, so every app and the scheduler see the allocation and degraded-boarding keys.
shared :: (Redis.HedisFlow m r, MonadFlow m) => m a -> m a
shared = Redis.runInMasterCloudRedisCellWithCrossAppRedis . Redis.withMasterRedis

-- | Waits for the lock, retrying every `delayMicros` plus up to half again of jitter, for at most 'lockWaitSec'; then fails with a
-- retriable error instead of holding the request. `operation` labels the wait in the datastore latency metric.
withOwnedLock :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> Int -> Int -> m a -> m a
withOwnedLock operation key ttlSec delayMicros action = do
  owner <- generateGUIDText
  deadline <- addUTCTime lockWaitSec <$> getCurrentTime
  let pause = liftIO (randomRIO (delayMicros, delayMicros + delayMicros `div` 2)) >>= threadDelay
  acquired <- withTimeRedis "sharedCab" operation (waitUntil getCurrentTime deadline (acquire key ttlSec owner) pause)
  unless acquired $ throwError (RedisLockStillProcessing key)
  action `finally` release key owner

lockWaitSec :: NominalDiffTime
lockWaitSec = 5

-- | Tries until `tryAcquire` succeeds or the clock has passed `deadline` after a failed try, pausing between tries.
waitUntil :: Monad m => m UTCTime -> UTCTime -> m Bool -> m () -> m Bool
waitUntil clock deadline tryAcquire pause = go
  where
    go =
      tryAcquire >>= \case
        True -> pure True
        False -> clock >>= \now -> if now >= deadline then pure False else pause >> go

-- | Nothing when another holder has the lock.
tryWithOwnedLock :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> Int -> m a -> m (Maybe a)
tryWithOwnedLock key ttlSec action = do
  owner <- generateGUIDText
  acquired <- acquire key ttlSec owner
  if acquired then (Just <$> action) `finally` release key owner else pure Nothing

acquire :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Int -> Text -> m Bool
acquire key ttlSec owner = (== Right RawRedis.Ok) <$> onLockKey key (\k -> acquireOwned k ttlSec owner)

release :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Text -> m ()
release key owner =
  withTryCatch "sharedCab:releaseLock" (onLockKey key (`releaseOwned` owner)) >>= \case
    Left err -> logError $ "sharedCab: could not release lock " <> key <> ": " <> show err
    Right _ -> pure ()

onLockKey :: (Redis.HedisFlow m r, MonadFlow m) => Text -> (ByteString -> RawRedis.Redis (Either RawRedis.Reply a)) -> m (Either RawRedis.Reply a)
onLockKey key = shared . Redis.runWithPrefixEither (Redis.buildLockResourceName key)

ownerValue :: Text -> ByteString
ownerValue = BSL.toStrict . A.encode

acquireOwned :: ByteString -> Int -> Text -> RawRedis.Redis (Either RawRedis.Reply RawRedis.Status)
acquireOwned key ttlSec owner = RawRedis.setOpts key (ownerValue owner) (RawRedis.SetOpts (Just (toInteger ttlSec)) Nothing (Just RawRedis.Nx))

releaseOwned :: ByteString -> Text -> RawRedis.Redis (Either RawRedis.Reply Integer)
releaseOwned key owner = RawRedis.eval releaseScript [key] [ownerValue owner]
  where
    releaseScript = "if redis.call('GET', KEYS[1]) == ARGV[1] then return redis.call('DEL', KEYS[1]) else return 0 end"
