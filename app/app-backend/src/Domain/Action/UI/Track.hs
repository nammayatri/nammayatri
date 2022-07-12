module Domain.Action.UI.Track
  ( track,
    DTrackRes (..),
  )
where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Ride as Ride
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import Utils.Common

data DTrackRes = TrackRes
  { bppRideId :: Id Ride.BPPRide,
    bppId :: Text,
    bppUrl :: BaseUrl
  }

track :: (EsqDBFlow m r) => Id Ride.Ride -> m DTrackRes
track rideId = do
  ride <- QR.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  return $
    TrackRes
      { bppRideId = ride.bppRideId,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl
      }
