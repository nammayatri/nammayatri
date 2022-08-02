module Domain.Action.UI.Feedback where

import qualified App.Types as App
import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Booking as DBooking
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import qualified Types.API.Feedback as API
import Types.Error
import Utils.Common

data DRatingReq = DRatingReq
  { bppBookingId :: Id DBooking.BPPBooking,
    ratingValue :: Int,
    feedbackDetails :: Text,
    providerId :: Text,
    providerUrl :: BaseUrl
  }

feedback :: API.FeedbackReq -> App.Flow DRatingReq
feedback request = do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
      feedbackDetails = request.feedbackDetails
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  pure
    DRatingReq
      { providerId = booking.providerId,
        providerUrl = booking.providerUrl,
        ..
      }
