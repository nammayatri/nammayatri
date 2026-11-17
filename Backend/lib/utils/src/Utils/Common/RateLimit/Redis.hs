module Utils.Common.RateLimit.Redis (incrWithExpireAtomic) where

import qualified Database.Redis as Hedis
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.TryException (TryException)

incrWithExpireAtomic :: (Redis.HedisFlow m env, TryException m) => Text -> Int -> m Integer
incrWithExpireAtomic key ttlSeconds =
  Redis.runWithPrefix key $ \prefKey ->
    Hedis.eval
      "local c = redis.call('INCR', KEYS[1]) if redis.call('TTL', KEYS[1]) < 0 then redis.call('EXPIRE', KEYS[1], ARGV[1]) end return c"
      [prefKey]
      [encodeUtf8 (show ttlSeconds :: Text)]
