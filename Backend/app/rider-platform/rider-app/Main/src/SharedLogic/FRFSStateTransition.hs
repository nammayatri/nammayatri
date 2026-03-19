module SharedLogic.FRFSStateTransition where

import Domain.Types.FRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

-- | Safe status update that validates the transition before persisting.
-- Logs a warning and skips if the transition is invalid, preventing state corruption.
safeUpdateBookingStatus ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Log m) =>
  FRFSTicketBookingStatus ->
  FRFSTicketBookingStatus ->
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  m Bool
safeUpdateBookingStatus currentStatus newStatus bookingId = do
  if isValidTransition currentStatus newStatus
    then do
      QFRFSTicketBooking.updateStatusById newStatus bookingId
      pure True
    else do
      logWarning $
        "FRFS State Transition Rejected: "
          <> show currentStatus
          <> " -> "
          <> show newStatus
          <> " for bookingId: "
          <> bookingId.getId
      pure False
