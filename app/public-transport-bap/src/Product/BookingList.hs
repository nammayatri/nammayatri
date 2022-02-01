module Product.BookingList where

import Beckn.Prelude
import Beckn.Utils.Common
import Domain.Booking.API
import Domain.Booking.Type
import Storage.Queries.Booking as QBooking
import Storage.Queries.PaymentTransaction as QPT
import qualified Storage.Queries.PublicTranport as QPublicTranport
import Tools.Auth
import Tools.Error

bookingListHandler :: EsqDBFlow m r => PersonId -> Integer -> Integer -> m BookingListRes
bookingListHandler personId limit offset = do
  bList <- QBooking.findAllByRequestorId personId limit offset
  BookingListRes
    <$> traverse buildBookingListRes bList

buildBookingListRes :: EsqDBFlow m r => Booking -> m BookingAPIEntity
buildBookingListRes booking = do
  departureStation <- QPublicTranport.findById booking.departureStationId >>= fromMaybeM PublicTransportNotFound
  arrivalStation <- QPublicTranport.findById booking.arrivalStationId >>= fromMaybeM PublicTransportNotFound
  paymentTrans <- QPT.findByBookingId booking.id
  return $ makeBookingAPIEntity booking departureStation arrivalStation paymentTrans