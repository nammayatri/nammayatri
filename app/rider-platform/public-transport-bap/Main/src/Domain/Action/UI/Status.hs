module Domain.Action.UI.Status where

import qualified Domain.Types.Booking as DBooking
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.Id
import Kernel.Utils.Common
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
