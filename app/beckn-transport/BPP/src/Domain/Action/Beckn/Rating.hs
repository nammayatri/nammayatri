module Domain.Action.Beckn.Rating where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Booking.Type as DRB
import qualified Domain.Types.Organization as Org
import Domain.Types.Person as SP
import Domain.Types.Rating as Rating
import qualified Domain.Types.Ride as Ride
import qualified EulerHS.Language as L
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as Rating
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics
import Types.Error
import Utils.Common

data DRatingReq = DRatingReq
  { bookingId :: Id DRB.Booking,
    ratingValue :: Int
  }

ratingImpl ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv
      m
      r
      '["minimumDriverRatesCount" ::: Int],
    CoreMetrics
      m
  ) =>
  Id Org.Organization ->
  DRatingReq ->
  m ()
ratingImpl transporterId req = do
  rideBooking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  unless (rideBooking.providerId == transporterId) $ throwError AccessDenied
  ride <-
    QRide.findActiveByRBId rideBooking.id
      >>= fromMaybeM (RideNotFound rideBooking.id.getId)
  let driverId = ride.driverId
  unless (ride.status == Ride.COMPLETED) $
    throwError $ RideInvalidStatus "Ride is not ready for rating."
  mbRating <- Rating.findByRideId ride.id
  case mbRating of
    Nothing -> do
      logTagInfo "FeedbackAPI" $
        "Creating a new record for " <> show ride.id <> " with rating " <> show req.ratingValue <> "."
      newRating <- mkRating ride.id driverId req.ratingValue
      Esq.runTransaction $ Rating.create newRating
    Just rating -> do
      logTagInfo "FeedbackAPI" $
        "Updating existing rating for " <> show ride.id <> " with new rating " <> show req.ratingValue <> "."
      Esq.runTransaction $ do
        Rating.updateRatingValue rating.id driverId req.ratingValue
  calculateAverageRating driverId

calculateAverageRating ::
  (EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["minimumDriverRatesCount" ::: Int]) =>
  Id SP.Person ->
  m ()
calculateAverageRating personId = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " <> show personId <> ""
  allRatings <- Rating.findAllRatingsForPerson personId
  let ratingsSum :: Double = fromIntegral $ sum (allRatings <&> (.ratingValue))
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  minimumDriverRatesCount <- asks (.minimumDriverRatesCount)
  when (ratingCount >= minimumDriverRatesCount) $ do
    let newAverage = ratingsSum / fromIntegral ratingCount
    logTagInfo "PersonAPI" $ "New average rating for person " <> show personId <> " , rating is " <> show newAverage <> ""
    Esq.runTransaction $ QP.updateAverageRating personId newAverage

mkRating :: MonadFlow m => Id Ride.Ride -> Id SP.Person -> Int -> m Rating.Rating
mkRating rideId driverId ratingValue = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ Rating.Rating {..}
