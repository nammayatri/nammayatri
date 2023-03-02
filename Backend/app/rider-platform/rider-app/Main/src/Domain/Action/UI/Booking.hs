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
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import Tools.Error

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatus :: forall m r. EsqDBReplicaFlow m r => Id SRB.Booking -> Id Person.Person -> m SRB.BookingAPIEntity
bookingStatus bookingId personId = do
  booking <- runInReplica (QRB.findById bookingId (Proxy @m)) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  SRB.buildBookingAPIEntity booking

bookingList :: forall m r. EsqDBReplicaFlow m r => Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> m BookingListRes
bookingList personId mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  rbList <- runInReplica $ QRB.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus (Proxy @m)
  BookingListRes <$> traverse SRB.buildBookingAPIEntity rbList
