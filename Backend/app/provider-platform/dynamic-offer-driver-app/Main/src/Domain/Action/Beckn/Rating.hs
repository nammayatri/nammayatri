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
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Prelude (head, (!!))
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as QRating
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data DRatingReq = DRatingReq
  { bookingId :: Id DBooking.Booking,
    ratingValue :: Int,
    feedbackDetails :: [Maybe Text]
  }

handler :: Id Merchant -> DRatingReq -> DRide.Ride -> Flow ()
handler merchantId req ride = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  rating <- B.runInReplica $ QRating.findRatingForRide ride.id
  let driverId = ride.driverId
  let ratingValue = req.ratingValue
      feedbackDetails = head req.feedbackDetails
      wasOfferedAssistance = case req.feedbackDetails !! 1 of
        Just "True" -> Just True
        Just "False" -> Just False
        _ -> Nothing
  _ <- case rating of
    Nothing -> do
      logTagInfo "FeedbackAPI" $
        "Creating a new record for " +|| ride.id ||+ " with rating " +|| ratingValue ||+ "."
      newRating <- buildRating ride.id driverId ratingValue feedbackDetails wasOfferedAssistance
      QRating.create newRating
    Just rideRating -> do
      logTagInfo "FeedbackAPI" $
        "Updating existing rating for " +|| ride.id ||+ " with new rating " +|| ratingValue ||+ "."
      QRating.updateRating rideRating.id driverId ratingValue feedbackDetails wasOfferedAssistance
  calculateAverageRating driverId merchant.minimumDriverRatesCount

calculateAverageRating ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Int ->
  m ()
calculateAverageRating personId minimumDriverRatesCount = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  allRatings <- B.runInReplica $ QRating.findAllRatingsForPerson personId
  let ratingsSum = fromIntegral $ sum (allRatings <&> (.ratingValue))
  let ratingCount = length allRatings
  when (ratingCount == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  when (ratingCount >= minimumDriverRatesCount) $ do
    let newAverage = ratingsSum / fromIntegral ratingCount
    logTagInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ " , rating is " +|| newAverage ||+ ""
    void $ QP.updateAverageRating personId newAverage

buildRating :: MonadFlow m => Id DRide.Ride -> Id DP.Person -> Int -> Maybe Text -> Maybe Bool -> m DRating.Rating
buildRating rideId driverId ratingValue feedbackDetails wasOfferedAssistance = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ DRating.Rating {..}

validateRequest :: DRatingReq -> Flow DRide.Ride
validateRequest req = do
  booking <- B.runInReplica $ QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  ride <-
    QRide.findActiveByRBId booking.id
      >>= fromMaybeM (RideNotFound booking.id.getId)
  unless (ride.status == DRide.COMPLETED) $
    throwError $ RideInvalidStatus "Ride is not ready for rating."
  return ride
