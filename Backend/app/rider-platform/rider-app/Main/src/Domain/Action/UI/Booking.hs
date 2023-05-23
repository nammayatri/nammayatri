{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Booking
  ( bookingStatus,
    bookingList,
  )
where

import qualified Domain.Action.UI.SimulatedFlow.Booking as SSBooking
import Domain.Types.Booking
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow, SimluatedCacheFlow)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import Tools.Error

bookingStatus :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, SimluatedCacheFlow m r) => Id SRB.Booking -> Id Person.Person -> m SRB.BookingAPIEntity
bookingStatus bookingId personId = do
  person <- runInReplica (QP.findById personId) >>= fromMaybeM (PersonNotFound personId.getId)
  SSBooking.bookingStatusSimulator person.isSimulated bookingId personId $ do
    booking <- runInReplica (QRB.findById bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
    unless (booking.riderId == personId) $ throwError AccessDenied
    SRB.buildBookingAPIEntity booking

bookingList :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, SimluatedCacheFlow m r) => Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> m BookingListRes
bookingList personId mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  person <- runInReplica (QP.findById personId) >>= fromMaybeM (PersonNotFound personId.getId)
  SSBooking.bookingListSimulator person.isSimulated mbBookingStatus personId do
    rbList <- runInReplica $ QRB.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus
    BookingListRes <$> traverse SRB.buildBookingAPIEntity rbList
