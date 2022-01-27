module Product.BookingList where

import Beckn.Prelude
import Beckn.Utils.Common
import Domain.Types.Booking.API
import Domain.Types.Booking.Type
import Storage.Queries.Booking as QBooking
import Storage.Queries.PaymentTransaction as QPT
import qualified Storage.Queries.TransportStation as QTransportStation
import Tools.Auth
import Tools.Error

bookingListHandler :: EsqDBFlow m r => PersonId -> Integer -> Integer -> m BookingListRes
bookingListHandler personId limit offset = do
  bList <- QBooking.findAllByRequestorId personId limit offset
  BookingListRes
    <$> traverse buildBookingListRes bList

buildBookingListRes :: EsqDBFlow m r => Booking -> m BookingAPIEntity
buildBookingListRes booking = do
  departureStation <- QTransportStation.findById booking.departureStationId >>= fromMaybeM TransportStationNotFound
  arrivalStation <- QTransportStation.findById booking.arrivalStationId >>= fromMaybeM TransportStationNotFound
  paymentTrans <- QPT.findByBookingId booking.id
  return $ makeBookingAPIEntity booking departureStation arrivalStation paymentTrans
