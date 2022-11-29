module Domain.Action.UI.Status where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Storage.Esqueleto.Transactionable (runInReplica)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as DBooking
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as QPT
import qualified Storage.Queries.TransportStation as QTransportStation
import Tools.Error

status :: EsqDBReplicaFlow m r => Id DBooking.Booking -> m DBooking.BookingAPIEntity
status bookingId = do
  booking <- runInReplica $ QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  departureStation <- runInReplica $ QTransportStation.findById booking.departureStationId >>= fromMaybeM TransportStationNotFound
  arrivalStation <- runInReplica $ QTransportStation.findById booking.arrivalStationId >>= fromMaybeM TransportStationNotFound
  paymentTrans <- runInReplica $ QPT.findByBookingId bookingId
  return $ DBooking.makeBookingAPIEntity booking departureStation arrivalStation paymentTrans
