-- | No-show / miss counters for every allocation release that `blameFor` charges to someone: a rider no-show on the booking
-- and the rider's person_stats, a driver miss on the trip the cab was running. No enforcement reads these yet.
module SharedLogic.SharedCab.NoShow
  ( Charge (..),
    chargeFor,
    noShowsAfter,
    NoShowAction (..),
    afterNoShow,
    actionAfterClose,
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

-- | What `blame` charges beyond the booking. Nobody is charged when the needed id is unknown (never the other party instead).
chargeFor :: Blame -> Maybe (Id DP.Person) -> Maybe (Id DVT.VehicleTrip) -> Maybe Charge
chargeFor BlameNone _ _ = Nothing
chargeFor BlameRider mbRider _ = ChargeRider <$> mbRider
chargeFor BlameDriver _ mbTrip = ChargeTrip <$> mbTrip

noShowsAfter :: Blame -> Int -> Int
noShowsAfter BlameRider n = n + 1
noShowsAfter _ n = n

data NoShowAction = Reallocate | AutoCancel
  deriving (Show, Eq)

afterNoShow :: Int -> Int -> NoShowAction
afterNoShow maxNoShows noShows
  | noShows >= maxNoShows = AutoCancel
  | otherwise = Reallocate

-- | What a close that blamed `blame` does to a booking with `noShows` already booked: only the rider's own no-show can
-- cancel it, and only if it takes the count to the cap.
actionAfterClose :: Blame -> Int -> Int -> NoShowAction
actionAfterClose blame maxNoShows noShows = case blame of
  BlameRider -> afterNoShow maxNoShows (noShowsAfter BlameRider noShows)
  BlameDriver -> Reallocate
  BlameNone -> Reallocate

record :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Blame -> Maybe (Id DP.Person) -> Maybe (Id DVT.VehicleTrip) -> m ()
record blame mbRider mbTrip =
  whenJust (chargeFor blame mbRider mbTrip) $ \case
    ChargeTrip tripId -> QVehicleTrip.incrementMissedPickups tripId
    ChargeRider personId ->
      QPersonStats.findByPersonId personId >>= \case
        Nothing -> logWarning $ "shared-cab no-show not counted: no person_stats row for " <> personId.getId
        Just _ -> QPersonStats.incrementSharedCabNoShows personId
