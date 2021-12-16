module API.Parking.Booking.BookingId.Handler where

import API.Parking.Booking.BookingId.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Booking as DBooking
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.ParkingLocation as QPLocation
import qualified Storage.Queries.PaymentTransaction as QPT
import Tools.Auth
import Tools.Error

handler :: FlowServer API
handler = status

status :: PersonId -> Id DBooking.Booking -> FlowHandler DBooking.BookingAPIEntity
status _ bookingId = withFlowHandlerAPI $ do
  booking <- QBooking.findById bookingId >>= fromMaybeM BookingDoesNotExist
  location <- QPLocation.findById (Id booking.parkingSpaceLocationId) >>= fromMaybeM ParkingLocationNotFound
  paymentTrans <- QPT.findByBookingId bookingId
  return $ DBooking.makeBookingAPIEntity booking location paymentTrans
