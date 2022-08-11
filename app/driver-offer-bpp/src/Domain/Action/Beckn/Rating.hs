module Domain.Action.Beckn.Rating where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person as SP
import Domain.Types.Rating as Rating
import qualified Domain.Types.Ride as Ride
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as Rating
import qualified Storage.Queries.Ride as QRide
import Types.Error
import Utils.Common

data DRatingReq = DRatingReq
  { bookingId :: Id DBooking.Booking,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text
  }

handler :: DRatingReq -> Flow ()
handler req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  ride <-
    QRide.findActiveByRBId booking.id
      >>= fromMaybeM (RideNotFound booking.id.getId)
  let driverId = ride.driverId
  unless (ride.status == Ride.COMPLETED) $
    throwError $ RideInvalidStatus "Ride is not ready for rating."
  let ratingValue = req.ratingValue
      feedbackDetails = req.feedbackDetails
  mbRating <- Rating.findByRideId ride.id
  case mbRating of
    Nothing -> do
      logTagInfo "FeedbackAPI" $
        "Creating a new record for " +|| ride.id ||+ " with rating " +|| ratingValue ||+ "."
      newRating <- buildRating ride.id driverId ratingValue feedbackDetails
      Esq.runTransaction $ Rating.create newRating
    Just rating -> do
      logTagInfo "FeedbackAPI" $
        "Updating existing rating for " +|| ride.id ||+ " with new rating " +|| ratingValue ||+ "."
      Esq.runTransaction $ do
        Rating.updateRating rating.id driverId ratingValue feedbackDetails
  calculateAverageRating driverId

calculateAverageRating ::
  (EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["minimumDriverRatesCount" ::: Int]) =>
  Id SP.Person ->
  m ()
calculateAverageRating personId = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- Rating.findAllRatingsForPerson personId
  let ratingsSum :: Double = fromIntegral $ sum (allRatings <&> (.ratingValue))
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  minimumDriverRatesCount <- asks (.minimumDriverRatesCount)
  when (ratingCount >= minimumDriverRatesCount) $ do
    let newAverage = ratingsSum / fromIntegral ratingCount
    logTagInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ " , rating is " +|| newAverage ||+ ""
    Esq.runTransaction $ QP.updateAverageRating personId newAverage

buildRating :: MonadFlow m => Id Ride.Ride -> Id SP.Person -> Int -> Maybe Text -> m Rating.Rating
buildRating rideId driverId ratingValue feedbackDetails = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ Rating.Rating {..}
