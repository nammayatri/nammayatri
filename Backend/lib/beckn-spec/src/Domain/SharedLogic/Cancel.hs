module Domain.SharedLogic.Cancel where

import Domain.Tools.Error
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common

tryCancellationLock :: (CacheFlow m r, EsqDBFlow m r) => Text -> m ()
tryCancellationLock transactionId = do
  isLockAcquired <- Redis.withCrossAppRedis $ Redis.tryLockRedis (mkCancellationLockKey transactionId) 30
  unless isLockAcquired $ throwError ActiveCancellationOngoing

releaseCancellationLock :: (CacheFlow m r, EsqDBFlow m r) => Text -> m ()
releaseCancellationLock transactionId = do
  Redis.withCrossAppRedis $ Redis.unlockRedis (mkCancellationLockKey transactionId)

mkCancellationLockKey :: Text -> Text
mkCancellationLockKey transactionId = "isBookingCancellable:" <> transactionId
