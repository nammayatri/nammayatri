{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Booking
  ( BookingListRes (..),
    bookingStatus,
    bookingList,
  )
where

import Data.OpenApi (ToSchema (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QR
import Tools.Error

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatus :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> m SRB.BookingAPIEntity
bookingStatus bookingId (personId, _) = do
  booking <- runInReplica (QRB.findById bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  logInfo $ "booking: test " <> show booking
  SRB.buildBookingAPIEntity booking

bookingList :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> m BookingListRes
bookingList (personId, _) mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  rbList <- runInReplica $ QR.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus
  rentalScheduledBookingList <- runInReplica $ QRB.findScheduledRentalBookings personId
  logInfo $ "rbList: test " <> show rbList
  BookingListRes <$> traverse SRB.buildBookingAPIEntity (rbList <> rentalScheduledBookingList)
