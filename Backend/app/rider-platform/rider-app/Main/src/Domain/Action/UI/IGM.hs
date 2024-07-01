{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.IGM where

import Domain.Types.Booking
import Domain.Types.Ride
import qualified IssueManagement.Common.UI.Issue as Common
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.Queries.Booking as QBooking
import Storage.Queries.Ride as QRide

data IGMIssueReq = IGMIssueReq
  { isValueAddNP :: Bool,
    ride :: Ride,
    booking :: Booking
  }
  deriving (Generic, Show)

buildIGMIssueReq :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Maybe (Id Common.Ride) -> m (Maybe IGMIssueReq)
buildIGMIssueReq mbRideId =
  case mbRideId of
    Just rideId -> do
      ride <- QRide.findById (cast rideId) >>= fromMaybeM (RideNotFound rideId.getId)
      booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      isValueAddNP <- CQVAN.isValueAddNP booking.providerId
      return $ Just IGMIssueReq {isValueAddNP, ride, booking}
    Nothing -> return Nothing
