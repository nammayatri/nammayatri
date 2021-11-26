module Product.BecknProvider.Rating where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Migration1.API.Rating as Rating
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as Rating
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Person as SP
import Types.Storage.Rating as Rating
  ( Rating,
    RatingT (..),
  )
import qualified Types.Storage.Ride as Ride
import Utils.Common

ratingImpl ::
  Id Organization ->
  SignatureAuthResult ->
  Rating.RatingReq ->
  FlowHandler AckResponse
ratingImpl _ _ req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "ratingAPI" "Received rating API call."
    let context = req.context
    validateContextMig1 context
    let rideBookingId = Id $ req.message.id
    rideBooking <- QRB.findById rideBookingId >>= fromMaybeM RideBookingDoesNotExist
    ride <-
      QRide.findByRBId rideBooking.id
        >>= fromMaybeM RideNotFound
    let driverId = ride.driverId
    unless (ride.status == Ride.COMPLETED) $
      throwError $ QuoteInvalidStatus "Order is not ready for rating."
    let ratingValue = req.message.value
    mbRating <- Rating.findByRideId ride.id
    case mbRating of
      Nothing -> do
        logTagInfo "FeedbackAPI" $
          "Creating a new record for " +|| ride.id ||+ " with rating " +|| ratingValue ||+ "."
        newRating <- mkRating ride.id driverId ratingValue
        Rating.create newRating
      Just rating -> do
        logTagInfo "FeedbackAPI" $
          "Updating existing rating for " +|| ride.id ||+ " with new rating " +|| ratingValue ||+ "."
        Rating.updateRatingValue rating.id driverId ratingValue
    calculateAverageRating driverId
    return Ack

calculateAverageRating ::
  (DBFlow m r, EncFlow m r, HasFlowEnv m r '["minimumDriverRatesCount" ::: Int]) =>
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
    QP.updateAverageRating personId newAverage

mkRating :: MonadFlow m => Id Ride.Ride -> Id SP.Person -> Int -> m Rating.Rating
mkRating rideId driverId ratingValue = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ Rating.Rating {..}
