{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.Rating where

import Data.Foldable ()
import Data.OpenApi
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import Domain.Types.RideStatus
import qualified Domain.Types.RideStatus as DRide
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import IssueManagement.Domain.Types.MediaFile as D
import Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.CacheFlow
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Rating as QRating
import qualified Storage.Queries.Ride as QRide
import Tools.Error (RatingError (..))

data FeedbackReq = FeedbackReq
  { rideId :: Id DRide.BPPRide,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text,
    wasOfferedAssistance :: Maybe Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance ToSchema FeedbackReq where
  declareNamedSchema proxy = do
    genericDeclareNamedSchema customSchemaOptions proxy
    where
      customSchemaOptions = defaultSchemaOptions {datatypeNameModifier = const "FeedbackReqInternal"}

rating ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["internalAPIKey" ::: Text]
  ) =>
  Maybe Text ->
  FeedbackReq ->
  m APISuccess
rating apiKey FeedbackReq {..} = do
  internalAPIKey <- asks (.internalAPIKey)
  ride <- B.runInReplica $ QRide.findByBPPRideId rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- B.runInReplica $ B.runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  person <- B.runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  merchant <- QMerc.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  _ <- validateRequest ratingValue ride.status
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

  ratingu <- B.runInReplica $ QRating.findRatingForRide ride.id
  _ <- case ratingu of
    Nothing -> do
      logTagInfo "FeedbackAPI" $
        "Creating a new record for " +|| ride.id ||+ " with rating " +|| ratingValue ||+ "."
      newRating <- buildRating ride.id ride.merchantId ride.merchantOperatingCityId booking.riderId ratingValue feedbackDetails wasOfferedAssistance Nothing
      QRating.create newRating
    Just rideRating -> do
      logTagInfo "FeedbackAPI" $
        "Updating existing rating for " +|| ride.id ||+ " with new rating " +|| ratingValue ||+ "."
      QRating.updateRating ratingValue feedbackDetails wasOfferedAssistance Nothing rideRating.id booking.riderId
  calculateAverageRating booking.riderId merchant.minimumDriverRatesCount ratingValue person.totalRatings person.totalRatingScore
  pure Success

calculateAverageRating ::
  (EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Int ->
  Int ->
  Int ->
  Int ->
  m ()
calculateAverageRating personId minimumDriverRatesCount ratingValue totalRatings totalRatingScore = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  let newRatingsCount = totalRatings + 1
  let newTotalRatingScore = totalRatingScore + ratingValue
  when (totalRatings == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  let isValidRating = newRatingsCount >= minimumDriverRatesCount
  logTagInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ ""
  void $ QP.updateAverageRating newRatingsCount newTotalRatingScore isValidRating personId

buildRating ::
  MonadFlow m =>
  Id DRide.Ride ->
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Id DP.Person ->
  Int ->
  Maybe Text ->
  Maybe Bool ->
  Maybe (Id D.MediaFile) ->
  m DRating.Rating
buildRating rideId merchantId merchantOperatingCityId riderId ratingValue feedbackDetails wasOfferedAssistance mediaId = do
  id <- Id <$> L.generateGUID
  now <- getCurrentTime
  let createdAt = now
  let updatedAt = now
  pure $ DRating.Rating {..}

validateRequest :: (MonadFlow m) => Int -> RideStatus -> m ()
validateRequest ratingValue rideStatus = do
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  unless (rideStatus == DRide.COMPLETED) $ throwError (RideInvalidStatus "Feedback available only for completed rides.")
