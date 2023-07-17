{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnStatus
  ( onStatus,
    OnStatusReq (..),
    RideInfo (..),
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide

data OnStatusReq = OnStatusReq
  { bppBookingId :: Id DBooking.BPPBooking,
    bookingStatus :: DBooking.BookingStatus,
    mbRideInfo :: Maybe RideInfo
  }

-- we can receive more rideInfo in later PRs
data RideInfo = RideInfo
  { bppRideId :: Id DRide.BPPRide,
    rideStatus :: DRide.RideStatus
  }

onStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => OnStatusReq -> m ()
onStatus OnStatusReq {..} = do
  booking <- QBooking.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  case mbRideInfo of
    Nothing -> do
      case booking.bookingDetails of
        DBooking.OneWaySpecialZoneDetails _ -> unless (booking.status == DBooking.CANCELLED) $ pure ()
        _ -> QBooking.updateStatus booking.id bookingStatus
    Just rideInfo -> do
      ride <- B.runInReplica $ QRide.findByBPPRideId rideInfo.bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId: " <> rideInfo.bppRideId.getId)
      QBooking.updateStatus booking.id bookingStatus
      QRide.updateStatus ride.id rideInfo.rideStatus

-- mapBookingStatusDomain :: DBooking.BookingStatus -> DBooking.BookingStatus
-- mapBookingStatusDomain DBooking.NEW = DBooking.CONFIRMED
-- mapBookingStatusDomain DBooking.TRIP_ASSIGNED = DBooking.TRIP_ASSIGNED
-- mapBookingStatusDomain DBooking.CONFIRMED = DBooking.CONFIRMED
-- mapBookingStatusDomain DBooking.COMPLETED = DBooking.COMPLETED
-- mapBookingStatusDomain DBooking.AWAITING_REASSIGNMENT = DBooking.AWAITING_REASSIGNMENT
-- mapBookingStatusDomain DBooking.REALLOCATED = DBooking.REALLOCATED
-- mapBookingStatusDomain DBooking.CANCELLED = DBooking.CANCELLED
