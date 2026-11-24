-- | Whole-record writes of an FRFS booking that was read earlier.
--
-- `updateByPrimaryKey` writes every column from the value in hand. The shuttle pass issue job sets the booking's
-- boarding OTP on its own, so a whole-record write built from an older read would put the old (usually empty)
-- OTP back. These writes keep the OTP that is stored at the time of the write instead.
module SharedLogic.FRFSBookingWrite
  ( updateBookingKeepingOtp,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

updateBookingKeepingOtp :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DBooking.FRFSTicketBooking -> m ()
updateBookingKeepingOtp booking = do
  mbStored <- QFRFSTicketBooking.findById booking.id
  let storedOtp = maybe booking.boardingOtp (.boardingOtp) mbStored
  QFRFSTicketBooking.updateByPrimaryKey booking {DBooking.boardingOtp = storedOtp}
