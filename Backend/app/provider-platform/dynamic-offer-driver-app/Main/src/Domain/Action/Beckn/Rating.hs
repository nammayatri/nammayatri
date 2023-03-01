{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Rating where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as QRating
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data DRatingReq = DRatingReq
  { bookingId :: Id DBooking.Booking,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text
  }

handler :: DRatingReq -> Flow ()
handler req = do
  booking <- QRB.findById req.bookingId (Proxy @Flow) >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  ride <-
    QRide.findActiveByRBId booking.id (Proxy @Flow)
      >>= fromMaybeM (RideNotFound booking.id.getId)
  rating <- QRating.findRatingForRide ride.id (Proxy @Flow)
  let driverId = ride.driverId
  unless (ride.status == DRide.COMPLETED) $
    throwError $ RideInvalidStatus "Ride is not ready for rating."
  let ratingValue = req.ratingValue
      feedbackDetails = req.feedbackDetails
  case rating of
    Nothing -> do
      logTagInfo "FeedbackAPI" $
        "Creating a new record for " +|| ride.id ||+ " with rating " +|| ratingValue ||+ "."
      newRating <- buildRating ride.id driverId ratingValue feedbackDetails
      Esq.runTransaction $ QRating.create @Flow newRating
    Just rideRating -> do
      logTagInfo "FeedbackAPI" $
        "Updating existing rating for " +|| ride.id ||+ " with new rating " +|| ratingValue ||+ "."
      Esq.runTransaction $ do
        QRating.updateRating @Flow rideRating.id driverId ratingValue feedbackDetails
  calculateAverageRating driverId

calculateAverageRating ::
  forall m r.
  (EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["minimumDriverRatesCount" ::: Int]) =>
  Id DP.Person ->
  m ()
calculateAverageRating personId = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- QRating.findAllRatingsForPerson personId (Proxy @m)
  let ratingsSum = fromIntegral $ sum (allRatings <&> (.ratingValue))
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  minimumDriverRatesCount <- asks (.minimumDriverRatesCount)
  when (ratingCount >= minimumDriverRatesCount) $ do
    let newAverage = ratingsSum / fromIntegral ratingCount
    logTagInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ " , rating is " +|| newAverage ||+ ""
    Esq.runTransaction $ QP.updateAverageRating @m personId newAverage

buildRating :: MonadFlow m => Id DRide.Ride -> Id DP.Person -> Int -> Maybe Text -> m DRating.Rating
buildRating rideId driverId ratingValue feedbackDetails = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ DRating.Rating {..}
