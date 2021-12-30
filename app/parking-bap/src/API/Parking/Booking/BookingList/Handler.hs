module API.Parking.Booking.BookingList.Handler where

import API.Parking.Booking.BookingList.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Booking.API as Booking
import Domain.Booking.Type
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.ParkingLocation as QPLocation
import qualified Storage.Queries.PaymentTransaction as QPT
import Tools.Auth
import Tools.Error

handler :: FlowServer API
handler = bookingList

bookingList :: PersonId -> Maybe Integer -> Maybe Integer -> FlowHandler Booking.BookingListRes
bookingList personId mbLimit mbOffset = withFlowHandlerAPI $ do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  bList <- QBooking.findAllByRequestorId personId limit offset
  Booking.BookingListRes <$> traverse buildBookingListRes bList

buildBookingListRes :: EsqDBFlow m r => Booking -> m Booking.BookingAPIEntity
buildBookingListRes booking = do
  location <- QPLocation.findById (Id booking.parkingSpaceLocationId) >>= fromMaybeM ParkingLocationNotFound
  paymentTrans <- QPT.findByBookingId booking.id
  return $ Booking.makeBookingAPIEntity booking location paymentTrans
