module Domain.Action.UI.Feedback
  ( FeedbackReq (..),
    FeedbackRes (..),
    feedback,
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as DRide
import qualified Environment as App
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Ride as QRide
import Tools.Error

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
    QPFS.updateStatus booking.riderId DPFS.IDLE
  pure
    FeedbackRes
      { providerId = booking.providerId,
        providerUrl = booking.providerUrl,
        ..
      }
