{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Status
  ( handler,
    DStatusReq (..),
    DStatusRes (..),
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide

newtype DStatusReq = StatusReq
  { rideId :: Id DRide.Ride
  }

data DStatusRes = StatusRes
  { transporter :: DM.Merchant,
    bookingId :: Id DBooking.Booking,
    bookingStatus :: DBooking.BookingStatus,
    rideStatus :: DRide.RideStatus
  }

handler ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  DStatusReq ->
  m DStatusRes
handler transporterId req = do
  transporter <-
    QM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied

  return $
    StatusRes
      { bookingId = booking.id,
        bookingStatus = booking.status,
        rideStatus = ride.status,
        ..
      }
