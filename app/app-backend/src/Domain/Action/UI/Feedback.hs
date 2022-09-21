module Domain.Action.UI.Feedback
  ( FeedbackReq (..),
    FeedbackRes (..),
    feedback,
  )
where

import qualified App.Types as App
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Id
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Ride as DRide
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Types.Error
import Utils.Common

data FeedbackReq = FeedbackReq
  { rideId :: Id DRide.Ride,
    rating :: Int,
    feedbackDetails :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data FeedbackRes = FeedbackRes
  { bppBookingId :: Id DBooking.BPPBooking,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text,
    providerId :: Text,
    providerUrl :: BaseUrl
  }

feedback :: FeedbackReq -> App.Flow FeedbackRes
feedback request = do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
      feedbackDetails = request.feedbackDetails
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus "Feedback available only for completed rides.")
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  DB.runTransaction $ do
    QRide.updateRideRating rideId ratingValue
  pure
    FeedbackRes
      { providerId = booking.providerId,
        providerUrl = booking.providerUrl,
        ..
      }
