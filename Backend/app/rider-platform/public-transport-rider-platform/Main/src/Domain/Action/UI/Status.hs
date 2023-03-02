{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

status :: forall m r. EsqDBReplicaFlow m r => Id DBooking.Booking -> m DBooking.BookingAPIEntity
status bookingId = do
  booking <- runInReplica $ QBooking.findById bookingId (Proxy @m) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  departureStation <- runInReplica $ QTransportStation.findById booking.departureStationId (Proxy @m) >>= fromMaybeM TransportStationNotFound
  arrivalStation <- runInReplica $ QTransportStation.findById booking.arrivalStationId (Proxy @m) >>= fromMaybeM TransportStationNotFound
  paymentTrans <- runInReplica $ QPT.findByBookingId bookingId (Proxy @m)
  return $ DBooking.makeBookingAPIEntity booking departureStation arrivalStation paymentTrans
