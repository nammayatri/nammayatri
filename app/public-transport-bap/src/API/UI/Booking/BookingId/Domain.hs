module API.UI.Booking.BookingId.Domain where

import API.UI.Booking.BookingId.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as DBooking
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as QPT
import qualified Storage.Queries.TransportStation as QTransportStation
import Tools.Auth
import Tools.Error

handler :: FlowServer API
handler = status

status :: PersonId -> Id DBooking.Booking -> FlowHandler DBooking.BookingAPIEntity
status _ bookingId = withFlowHandlerAPI $ do
  booking <- QBooking.findById bookingId >>= fromMaybeM BookingDoesNotExist
  departureStation <- QTransportStation.findById booking.departureStationId >>= fromMaybeM TransportStationNotFound
  arrivalStation <- QTransportStation.findById booking.arrivalStationId >>= fromMaybeM TransportStationNotFound
  paymentTrans <- QPT.findByBookingId bookingId
  return $ DBooking.makeBookingAPIEntity booking departureStation arrivalStation paymentTrans
