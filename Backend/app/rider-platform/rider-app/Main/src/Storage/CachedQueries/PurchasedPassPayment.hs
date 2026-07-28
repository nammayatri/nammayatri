module Storage.CachedQueries.PurchasedPassPayment
  ( setHasPasses,
    getHasPasses,
    clearHasPasses,
  )
where

import Data.Time.Calendar (Day, addDays)
import Data.Time.Clock (UTCTime (..))
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Second, independent signal that a person holds a pass, alongside
-- @Person.hasPass@ (PG + KV) and the client's @x-has-passes@ header.
--
-- The three are written at deliberately different moments -- the column at
-- purchase, this key on pass-list -- so a single failed flow cannot take out
-- both. A false negative on the read path requires all three to be wrong at once.
makeHasPassesKey :: Id DP.Person -> Text
makeHasPassesKey personId = "PurchasedPassPayment:HasPasses-" <> personId.getId

-- | Buffer held on top of the pass end date, so the flag comfortably outlives
-- the pass it was set for.
hasPassesTtlBuffer :: Int
hasPassesTtlBuffer = 7 * 24 * 60 * 60

-- | Set the flag with a TTL derived from the pass end date, never shortening an
-- existing one. Pass validity is per-pass config ('maxValidDays'), so a fixed TTL
-- would silently expire under a long-lived pass; deriving it keeps the flag valid
-- for as long as any pass the person holds, and renewals extend it.
setHasPasses :: (CacheFlow m r, MonadFlow m) => Id DP.Person -> Day -> m ()
setHasPasses personId endDate = do
  now <- getCurrentTime
  let key = makeHasPassesKey personId
      -- end of the pass's last valid day, plus buffer
      secondsTillEnd = max 0 . round $ diffUTCTime (UTCTime (addDays 1 endDate) 0) now
      newTtl = secondsTillEnd + hasPassesTtlBuffer
  -- ttl returns a negative sentinel when the key is missing or has no expiry,
  -- so `max` naturally falls back to the freshly computed value.
  existingTtl <- fromInteger <$> Hedis.ttl key
  Hedis.setExp key True (max newTtl existingTtl)

getHasPasses :: (CacheFlow m r) => Id DP.Person -> m (Maybe Bool)
getHasPasses personId = Hedis.safeGet (makeHasPassesKey personId)

clearHasPasses :: (CacheFlow m r) => Id DP.Person -> m ()
clearHasPasses personId = Hedis.del (makeHasPassesKey personId)
