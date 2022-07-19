module Domain.Action.UI.Feedback where

import qualified App.Types as App
import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as DRideBooking
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Types.API.Feedback as API
import Types.Error
import Utils.Common

data DRatingReq = DRatingReq
  { bppRideBookingId :: Id DRideBooking.BPPRideBooking,
    ratingValue :: Int,
    providerId :: Text,
    providerUrl :: BaseUrl
  }

feedback :: API.FeedbackReq -> App.Flow DRatingReq
feedback request = do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  bppRideBookingId <- rideBooking.bppBookingId & fromMaybeM (RideBookingFieldNotPresent "bppBookingId")
  pure
    DRatingReq
      { providerId = rideBooking.providerId,
        providerUrl = rideBooking.providerUrl,
        ..
      }
