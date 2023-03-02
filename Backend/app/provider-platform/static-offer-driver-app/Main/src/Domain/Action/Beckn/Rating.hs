{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Rating where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.App
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as QRating
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Metrics

data DRatingReq = DRatingReq
  { bookingId :: Id DRB.Booking,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text
  }

ratingImpl ::
  forall m r.
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["minimumDriverRatesCount" ::: Int],
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  DRatingReq ->
  m ()
ratingImpl transporterId req = do
  booking <- QRB.findById req.bookingId (Proxy @m) >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  unless (booking.providerId == transporterId) $ throwError AccessDenied
  ride <-
    QRide.findActiveByRBId booking.id (Proxy @m)
      >>= fromMaybeM (RideNotFound booking.id.getId)
  let driverId = ride.driverId
  unless (ride.status == DRide.COMPLETED) $
    throwError $ RideInvalidStatus "Ride is not ready for rating."
  case ride.rideRating of
    Nothing -> do
      logTagInfo "FeedbackAPI" $
        "Creating a new record for " <> show ride.id <> " with rating " <> show req.ratingValue <> "."
      newRating <- mkRating ride.id driverId req.ratingValue req.feedbackDetails
      Esq.runTransaction $ QRating.create @m newRating
    Just rideRating -> do
      logTagInfo "FeedbackAPI" $
        "Updating existing rating for " <> show ride.id <> " with new rating " <> show req.ratingValue <> "."
      let ratingId = cast @DRide.RideRating @DRating.Rating rideRating.id
      Esq.runTransaction $ do
        QRating.updateRating @m ratingId driverId req.ratingValue req.feedbackDetails
  calculateAverageRating driverId

calculateAverageRating ::
  forall m r.
  (EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["minimumDriverRatesCount" ::: Int]) =>
  Id DP.Person ->
  m ()
calculateAverageRating personId = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " <> show personId <> ""
  allRatings <- QRating.findAllRatingsForPerson personId (Proxy @m)
  let ratingsSum = fromIntegral $ sum (allRatings <&> (.ratingValue))
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  minimumDriverRatesCount <- asks (.minimumDriverRatesCount)
  when (ratingCount >= minimumDriverRatesCount) $ do
    let newAverage = ratingsSum / fromIntegral ratingCount
    logTagInfo "PersonAPI" $ "New average rating for person " <> show personId <> " , rating is " <> show newAverage <> ""
    Esq.runTransaction $ QP.updateAverageRating @m personId newAverage

mkRating :: MonadFlow m => Id DRide.Ride -> Id DP.Person -> Int -> Maybe Text -> m DRating.Rating
mkRating rideId driverId ratingValue feedbackDetails = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ DRating.Rating {..}
