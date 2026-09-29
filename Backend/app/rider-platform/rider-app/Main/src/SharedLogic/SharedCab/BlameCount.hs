-- | R18: lifetime no-show / miss counters (05 §8.4), bumped from Allocation.afterClose on every
-- allocation release that `blameFor` charges to someone. Postgres, not Redis (the bump is a raw
-- atomic upsert, which a KV write would bypass) -- kept out
-- of KV the same way vehicle_trip is (dev/ddl-migrations/rider-app/1576-vehicle-trip-disable-kv.sql;
-- this table's twin is 1581-shared-cab-blame-count-disable-kv.sql).
--
-- No enforcement reads these yet; product sets thresholds later. `findTop` (spec query) is the
-- only consumer today, for ops.
module SharedLogic.SharedCab.BlameCount
  ( bump,
    subjectFor,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SharedCabBlameCount as DBlame
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (Blame (..))
import qualified Storage.Queries.SharedCabBlameCountExtra as QBlameExtra

-- | Which counter (subject type + id + merchant) `blame` charges: the rider (from the booking) or
-- the driver the allocation was made to (R45: captured at claim, not whoever holds the plate by now). Plain tuples, not the full records -- pure, and the only branch
-- in `bump`, so this is the falsifiable unit: swap in the wrong `Blame` and the wrong tuple wins.
subjectFor :: Blame -> Maybe (Text, Id DM.Merchant) -> Maybe (Text, Id DM.Merchant) -> Maybe (DBlame.BlameSubjectType, Text, Id DM.Merchant)
subjectFor BlameNone _ _ = Nothing
subjectFor BlameRider mbRider _ = (\(riderId, merchantId) -> (DBlame.RIDER_NO_SHOW, riderId, merchantId)) <$> mbRider
subjectFor BlameDriver _ mbDriver = (\(driverId, merchantId) -> (DBlame.DRIVER_MISS, driverId, merchantId)) <$> mbDriver

-- | Never thrown by the caller's contract: called from `afterClose`, outside every allocation lock, so
-- the increment is one atomic upsert (QBlameExtra.bump), not a read-modify-write.
bump ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Log m) =>
  Id DMOC.MerchantOperatingCity ->
  Blame ->
  Maybe DFTB.FRFSTicketBooking ->
  Maybe Text ->
  Id DFTB.FRFSTicketBooking ->
  UTCTime ->
  m ()
bump cityId blame mbBooking heldBy bookingId now = do
  let mbRider = (\b -> (b.riderId.getId, b.merchantId)) <$> mbBooking
      mbDriver = (,) <$> heldBy <*> (mbBooking <&> (.merchantId))
  whenJust (subjectFor blame mbRider mbDriver) $ \(subjectType, subjectId, merchantId) -> do
    newId <- generateGUID
    QBlameExtra.bump
      DBlame.SharedCabBlameCount
        { id = newId,
          subjectType,
          subjectId,
          merchantId,
          merchantOperatingCityId = cityId,
          count = 1,
          lastAt = now,
          lastBookingId = bookingId.getId,
          createdAt = now,
          updatedAt = now
        }
