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
import qualified Domain.Types.FRFSTicketBooking as FRFSTicketBooking
import Domain.Types.Ride
import qualified IssueManagement.Common.UI.Issue as Common
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import Storage.Queries.Ride as QRide
import Tools.Error

data IGMIssueReq = IGMIssueReq
  { ride :: Ride,
    booking :: Booking
  }
  deriving (Generic)

newtype FRFSIGMIssueReq = FRFSIGMIssueReq
  { ticketBooking :: FRFSTicketBooking.FRFSTicketBooking
  }
  deriving (Generic)

buildIGMIssueReq :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Maybe (Id Common.Ride) -> Maybe (Id Common.TicketBooking) -> m (Maybe (Either FRFSIGMIssueReq IGMIssueReq))
buildIGMIssueReq mbRideId mbTicketBookingId = case (mbRideId, mbTicketBookingId) of
  (Just rideId, _) -> do
    ride <- QRide.findById (cast rideId) >>= fromMaybeM (RideNotFound rideId.getId)
    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    CQVAN.isValueAddNP booking.providerId >>= \case
      True -> pure Nothing
      False -> pure $ Just $ Right $ IGMIssueReq {ride, booking}
  (_, Just ticketBookingId) -> do
    ticketBooking <- QFTB.findById (cast ticketBookingId) >>= fromMaybeM (TicketBookingNotFound ticketBookingId.getId)
    pure $ Just $ Left $ FRFSIGMIssueReq {ticketBooking = ticketBooking}
  _ -> pure Nothing
