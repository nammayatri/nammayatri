module ExternalBPP.EBIX.ExternalAPI.Order where

import qualified Data.UUID as UU
import Domain.Types.FRFSTicketBooking
import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error

-- This should be max 20 characters UUID (Using Transaction UUID)
getOrderId :: (MonadFlow m) => FRFSTicketBooking -> m Text
getOrderId booking = do
  bookingUUID <- UU.fromText booking.id.getId & fromMaybeM (InternalError "Booking Id not being able to parse into UUID")
  let bookingUUIDInt :: Integer = fromIntegral ((\(a, b, c, d) -> a + b + c + d) (UU.toWords bookingUUID))
  return $ show bookingUUIDInt
