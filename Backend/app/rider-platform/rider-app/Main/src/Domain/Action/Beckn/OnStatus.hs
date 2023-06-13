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
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (fromMaybeM)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data OnStatusReq = OnStatusReq
  { bppBookingId :: Id DBooking.BPPBooking,
    bookingStatus :: DBooking.BookingStatus,
    bppRideId :: Id DRide.BPPRide,
    rideStatus :: DRide.RideStatus
  }

onStatus :: EsqDBFlow m r => OnStatusReq -> m ()
onStatus OnStatusReq {..} = do
  booking <- QBooking.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId: " <> bppRideId.getId)
  DB.runTransaction $ do
    QBooking.updateStatus booking.id bookingStatus
    QRide.updateStatus ride.id rideStatus
