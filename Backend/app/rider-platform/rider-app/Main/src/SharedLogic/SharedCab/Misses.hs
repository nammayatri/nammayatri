-- | R18: no-show / miss counters (05 §8.4) for every allocation release that `blameFor` charges to someone.
-- A rider no-show is counted on the booking (`frfs_ticket_booking.shared_cab_no_shows`, inside `closeLocked`,
-- under the booking lock) and on the rider (`person_stats.shared_cab_no_shows`); a driver miss on the trip the cab
-- was running when the allocation was made (`vehicle_trip.missed_pickups`). No enforcement reads these yet.
module SharedLogic.SharedCab.Misses
  ( Charge (..),
    chargeFor,
    noShowsAfter,
    record,
  )
where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (Blame (..))
import qualified Storage.Queries.PersonStats as QPersonStats
import qualified Storage.Queries.VehicleTrip as QVehicleTrip

data Charge = ChargeRider (Id DP.Person) | ChargeTrip (Id DVT.VehicleTrip)
  deriving (Show, Eq)

-- | What `blame` charges beyond the booking. Pure, and the only branch in `record`: the falsifiable unit.
-- Nobody is charged when the needed id is unknown (never the other party instead).
chargeFor :: Blame -> Maybe (Id DP.Person) -> Maybe (Id DVT.VehicleTrip) -> Maybe Charge
chargeFor BlameNone _ _ = Nothing
chargeFor BlameRider mbRider _ = ChargeRider <$> mbRider
chargeFor BlameDriver _ mbTrip = ChargeTrip <$> mbTrip

-- | The booking's counter after a close that blamed `blame`: only a rider no-show moves it.
noShowsAfter :: Blame -> Int -> Int
noShowsAfter BlameRider n = n + 1
noShowsAfter _ n = n

-- | Called from `afterClose`, outside every allocation lock: each bump is one atomic SQL increment, not a read-modify-write.
record :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Blame -> Maybe (Id DP.Person) -> Maybe (Id DVT.VehicleTrip) -> m ()
record blame mbRider mbTrip =
  whenJust (chargeFor blame mbRider mbTrip) $ \case
    ChargeTrip tripId -> QVehicleTrip.incrementMissedPickups tripId
    ChargeRider personId ->
      QPersonStats.findByPersonId personId >>= \case
        Nothing -> logWarning $ "shared-cab no-show not counted: no person_stats row for " <> personId.getId
        Just _ -> QPersonStats.incrementSharedCabNoShows personId
