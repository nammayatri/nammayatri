module SharedLogic.CacheDistance where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common

cacheDistance ::
  (Redis.HedisFlow m r, MonadTime m, MonadFlow m) =>
  Text ->
  (Meters, Seconds) ->
  m ()
cacheDistance transactionId distance = Redis.setExp (distanceKey transactionId) distance 7200

getCacheDistance ::
  ( Redis.HedisFlow m r
  ) =>
  Text ->
  m (Maybe (Meters, Seconds))
getCacheDistance transactionId = Redis.get @(Meters, Seconds) (distanceKey transactionId)

distanceKey :: Text -> Text
distanceKey = ("distanceKey:" <>)
