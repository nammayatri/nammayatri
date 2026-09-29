-- | R18: per-run miss / no-show counters (05 §8.4), bumped from Allocation.afterClose on every allocation
-- release that `blameFor` charges to someone. A driver miss lands on the vehicle_trip the missing driver
-- was running (`vehicle_trip.missed_pickups`), a rider no-show on the booking (`frfs_ticket_booking.no_show_count`).
-- No enforcement reads these yet; product sets thresholds later.
module SharedLogic.SharedCab.Misses
  ( Charge (..),
    chargeFor,
    record,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Allocation.Types (Blame (..))
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.VehicleTrip as QVehicleTrip

data Charge = ChargeTrip (Id DVT.VehicleTrip) | ChargeBooking (Id DFTB.FRFSTicketBooking)
  deriving (Show, Eq)

-- | What `blame` charges. A driver miss goes to the plate's live trip only if that trip's driver is the one the
-- allocation was made to (R45: `heldBy`, captured at claim); if the plate changed hands since, nobody is charged
-- rather than the new driver. Pure, and the only branch in `record`: the falsifiable unit.
chargeFor :: Blame -> Id DFTB.FRFSTicketBooking -> Maybe Text -> Maybe (Text, Id DVT.VehicleTrip) -> Maybe Charge
chargeFor BlameNone _ _ _ = Nothing
chargeFor BlameRider bookingId _ _ = Just (ChargeBooking bookingId)
chargeFor BlameDriver _ heldBy sessionTrip = case (heldBy, sessionTrip) of
  (Just driverId, Just (sessionDriverId, tripId)) | driverId == sessionDriverId -> Just (ChargeTrip tripId)
  _ -> Nothing

-- | Called from `afterClose`, outside every allocation lock: each bump is one atomic SQL increment, not a read-modify-write.
record :: (MonadFlow m, EsqDBFlow m r) => Blame -> Id DFTB.FRFSTicketBooking -> Maybe Text -> Maybe (Text, Id DVT.VehicleTrip) -> m ()
record blame bookingId heldBy sessionTrip =
  whenJust (chargeFor blame bookingId heldBy sessionTrip) $ \case
    ChargeTrip tripId -> QVehicleTrip.incrementMissedPickups tripId
    ChargeBooking bId -> QFRFSTicketBooking.incrementNoShowCount bId
