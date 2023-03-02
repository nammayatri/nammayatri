{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.BookingList where

import Domain.Types.Booking.API
import Domain.Types.Booking.Type
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.Booking as QBooking
import Storage.Queries.PaymentTransaction as QPT
import qualified Storage.Queries.TransportStation as QTransportStation
import Tools.Auth
import Tools.Error

bookingListHandler :: forall m r. EsqDBReplicaFlow m r => PersonId -> Maybe Integer -> Maybe Integer -> Maybe BookingStatus -> m BookingListRes
bookingListHandler personId mbLimit mbOffset mbBookingStatus = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  logDebug $ getId personId
  bList <- runInReplica $ QBooking.findAllByRequestorId personId limit offset mbBookingStatus (Proxy @m)
  logDebug $ show bList
  BookingListRes
    <$> traverse buildBookingListRes bList

buildBookingListRes :: forall m r. EsqDBReplicaFlow m r => Booking -> m BookingAPIEntity
buildBookingListRes booking = do
  departureStation <- runInReplica $ QTransportStation.findById booking.departureStationId (Proxy @m) >>= fromMaybeM TransportStationNotFound
  arrivalStation <- runInReplica $ QTransportStation.findById booking.arrivalStationId (Proxy @m) >>= fromMaybeM TransportStationNotFound
  paymentTrans <- runInReplica $ QPT.findByBookingId booking.id (Proxy @m)
  return $ makeBookingAPIEntity booking departureStation arrivalStation paymentTrans
