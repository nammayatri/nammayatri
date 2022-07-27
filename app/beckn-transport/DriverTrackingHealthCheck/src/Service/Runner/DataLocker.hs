module Service.Runner.DataLocker where

import Beckn.Prelude
import Beckn.Storage.Redis.Queries (tryLockRedis, unlockRedis)
import Beckn.Types.Common
import Environment (Flow)
import Utils.Common

--TODO: Make DataLocker util in shared-kernel
withLock :: Text -> Flow () -> Flow ()
withLock serviceName f = do
  getLock
  f `catch` (log ERROR . makeLogSomeException)
  unlockRedis lockKey
  where
    getLock = do
      lockAvailable <- tryLockRedis lockKey 10
      unless lockAvailable getLock
    lockKey = "beckn:" <> serviceName <> ":lock"
