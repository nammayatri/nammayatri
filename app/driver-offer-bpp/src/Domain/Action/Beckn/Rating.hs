module Domain.Action.Beckn.Rating where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Person as SP
import Domain.Types.Rating as Rating
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as DRideBooking
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as Rating
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import Utils.Common

data DRatingReq = DRatingReq
  { rideBookingId :: Id DRideBooking.RideBooking,
    ratingValue :: Int
  }

handler :: DRatingReq -> Flow ()
handler req = do
  rideBooking <- QRB.findById req.rideBookingId >>= fromMaybeM (RideBookingDoesNotExist req.rideBookingId.getId)
  ride <-
    QRide.findActiveByRBId rideBooking.id
      >>= fromMaybeM (RideNotFound rideBooking.id.getId)
  let driverId = ride.driverId
  unless (ride.status == Ride.COMPLETED) $
    throwError $ RideInvalidStatus "Ride is not ready for rating."
  let ratingValue = req.ratingValue
  mbRating <- Rating.findByRideId ride.id
  case mbRating of
    Nothing -> do
      logTagInfo "FeedbackAPI" $
        "Creating a new record for " +|| ride.id ||+ " with rating " +|| ratingValue ||+ "."
      newRating <- mkRating ride.id driverId ratingValue
      Esq.runTransaction $ Rating.create newRating
    Just rating -> do
      logTagInfo "FeedbackAPI" $
        "Updating existing rating for " +|| ride.id ||+ " with new rating " +|| ratingValue ||+ "."
      Esq.runTransaction $ do
        Rating.updateRatingValue rating.id driverId ratingValue
  -- QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature) -- move to API layer
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

mkRating :: MonadFlow m => Id Ride.Ride -> Id SP.Person -> Int -> m Rating.Rating
mkRating rideId driverId ratingValue = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ Rating.Rating {..}
