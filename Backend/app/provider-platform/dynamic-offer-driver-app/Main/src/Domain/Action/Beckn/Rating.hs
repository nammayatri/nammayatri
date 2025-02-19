{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Rating where

import qualified AWS.S3 as S3
import Data.List.Extra ((!?))
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Domain.Types.Booking as DBooking
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDriverCorrelation as RDCD
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import IssueManagement.Domain.Types.MediaFile as D
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (encrypt)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import Storage.Beam.IssueManagement ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.DriverStats as SQD
import Storage.Queries.Person as SQP
import qualified Storage.Queries.Rating as QRating
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDriverCorrelation as RDC
import Tools.Error
import Tools.Notifications

data DRatingReq = DRatingReq
  { bookingId :: Id DBooking.Booking,
    ratingValue :: Int,
    feedbackDetails :: [Maybe Text],
    shouldFavDriver :: Maybe Bool,
    riderPhoneNum :: Maybe Text,
    filePath :: Maybe Text,
    riderName :: Maybe Text
  }

handler :: Id Merchant -> DRatingReq -> DRide.Ride -> Flow ()
handler merchantId req ride = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  let driverId = ride.driverId
  let ratingValue = req.ratingValue
      feedbackDetails = fromMaybe Nothing (listToMaybe req.feedbackDetails)
      wasOfferedAssistance = case fromMaybe Nothing (req.feedbackDetails !? 1) of
        Just "True" -> Just True
        Just "False" -> Just False
        _ -> Nothing
      issueId = fromMaybe Nothing (req.feedbackDetails !? 2)
      isSafe = Just $ isNothing issueId
  mbBooking <- QRB.findById req.bookingId
  driver <- SQP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  case mbBooking of
    Just booking -> do
      whenJust (liftA3 (,,) req.shouldFavDriver (getId <$> booking.riderId) req.riderPhoneNum) $ \(shouldFavDriver', riderId, riderPhoneNum) -> do
        when shouldFavDriver' $ do
          mbCorrelationRes <- RDC.findByRiderIdAndDriverId (Id riderId) ride.driverId
          case mbCorrelationRes of
            Just correlationRes -> do
              when (not correlationRes.favourite) $ do
                mbMerchantPN_ <- CPN.findMatchingMerchantPNInRideFlow ride.merchantOperatingCityId "FAVOURITE_DRIVER_ALERT" Nothing Nothing driver.language booking.configInExperimentVersions
                whenJust mbMerchantPN_ $ \merchantPN_ -> do
                  let title = T.replace "{#riderName#}" (fromMaybe "" req.riderName) merchantPN_.title
                      entityData = NotifReq {entityId = driverId.getId, title = title, message = merchantPN_.body}
                  notifyDriverOnEvents ride.merchantOperatingCityId driver.id driver.deviceToken entityData merchantPN_.fcmNotificationType
                RDC.updateFavouriteDriverForRider True (Id riderId) ride.driverId
                SQD.incFavouriteRiderCount ride.driverId
            Nothing -> do
              mbMerchantPN_ <- CPN.findMatchingMerchantPNInRideFlow ride.merchantOperatingCityId "FAVOURITE_DRIVER_ALERT" Nothing Nothing driver.language booking.configInExperimentVersions
              whenJust mbMerchantPN_ $ \merchantPN_ -> do
                let title = T.replace "{#riderName#}" (fromMaybe "" req.riderName) merchantPN_.title
                    entityData = NotifReq {entityId = driverId.getId, title = title, message = merchantPN_.body}
                notifyDriverOnEvents ride.merchantOperatingCityId driver.id driver.deviceToken entityData merchantPN_.fcmNotificationType
              now <- getCurrentTime
              encPhoneNumber <- encrypt riderPhoneNum
              let riderDriverCorr =
                    RDCD.RiderDriverCorrelation
                      { riderDetailId = Id riderId,
                        driverId = ride.driverId,
                        merchantId = merchantId,
                        merchantOperatingCityId = booking.merchantOperatingCityId,
                        createdAt = now,
                        updatedAt = now,
                        favourite = True,
                        mobileNumber = encPhoneNumber
                      }
              RDC.create riderDriverCorr
              SQD.incFavouriteRiderCount ride.driverId
    Nothing -> do
      logError $ "Booking not found for bookingId : " <> req.bookingId.getId
      pure ()

  rating' <- B.runInReplica $ QRating.checkIfRatingExistsForDriver ride.driverId
  driverStats <- runInReplica $ QDriverStats.findById ride.driverId >>= fromMaybeM DriverInfoNotFound

  -- backfilling rating for the old driver entries
  (ratingCount, ratingsSum) <- do
    if ((not $ null rating') && (isNothing driverStats.totalRatings) && (isNothing driverStats.totalRatingScore) && (isNothing driverStats.isValidRating))
      then do
        allRatings <- QRating.findAllRatingsForPerson driverId
        let ratingsSum = sum (allRatings <&> (.ratingValue))
        let ratingCount = length allRatings
        let isValidRating = ratingCount >= merchant.minimumDriverRatesCount
        QDriverStats.updateAverageRating driverId (Just ratingCount) (Just ratingsSum) (Just isValidRating)
        return (Just ratingCount, Just ratingsSum)
      else return (driverStats.totalRatings, driverStats.totalRatingScore)

  rating <- B.runInReplica $ QRating.findRatingForRide ride.id
  mediaId <- audioFeedbackUpload mbBooking req.filePath
  _ <- case rating of
    Nothing -> do
      logTagInfo "FeedbackAPI" $
        "Creating a new record for " +|| ride.id ||+ " with rating " +|| ratingValue ||+ "."
      newRating <- buildRating ride.merchantId (Just ride.merchantOperatingCityId) ride.id driverId ratingValue feedbackDetails issueId isSafe wasOfferedAssistance req.shouldFavDriver mediaId
      QRating.create newRating
    Just rideRating -> do
      logTagInfo "FeedbackAPI" $
        "Updating existing rating for " +|| ride.id ||+ " with new rating " +|| ratingValue ||+ "."
      QRating.updateRating ratingValue feedbackDetails isSafe issueId wasOfferedAssistance req.shouldFavDriver mediaId rideRating.id driverId
  logDebug "Driver Rating Coin Event"
  fork "DriverCoinRating Event" $ do
    DC.driverCoinsEvent driverId merchantId ride.merchantOperatingCityId (DCT.Rating ratingValue ride) (Just ride.id.getId) ride.vehicleVariant
  calculateAverageRating driverId merchant.minimumDriverRatesCount ratingValue ratingCount ratingsSum

calculateAverageRating ::
  (CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Int ->
  Int ->
  Maybe Int ->
  Maybe Int ->
  m ()
calculateAverageRating personId minimumDriverRatesCount ratingValue mbtotalRatings mbtotalRatingScore = do
  logTagInfo "PersonAPI" $ "Recalculating average rating for driver " +|| personId ||+ ""
  let totalRatings = fromMaybe 0 mbtotalRatings
  let totalRatingScore = fromMaybe 0 mbtotalRatingScore
  let newRatingsCount = totalRatings + 1
  let newTotalRatingScore = totalRatingScore + ratingValue
  when (totalRatings == 0) $
    logTagInfo "PersonAPI" "No rating found to calculate"
  let isValidRating = newRatingsCount >= minimumDriverRatesCount
  logTagInfo "PersonAPI" $ "New average rating for person " +|| personId ||+ ""
  void $ QDriverStats.updateAverageRating personId (Just newRatingsCount) (Just newTotalRatingScore) (Just isValidRating)

buildRating ::
  MonadFlow m =>
  Maybe (Id Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Id DRide.Ride ->
  Id DP.Person ->
  Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe (Id D.MediaFile) ->
  m DRating.Rating
buildRating merchantId merchantOperatingCityId rideId driverId ratingValue feedbackDetails issueId isSafe wasOfferedAssistance isFavourite mediaId = do
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
    throwError $ RideInvalidStatus ("Ride is not ready for rating." <> Text.pack (show ride.status))
  return ride

audioFeedbackUpload :: Maybe DBooking.Booking -> Maybe Text -> Flow (Maybe (Id D.MediaFile))
audioFeedbackUpload mbBooking mbFilePath = do
  case (mbBooking, mbFilePath) of
    (Just booking, Just filePath) -> do
      transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (MerchantNotFound booking.merchantOperatingCityId.getId)
      let fileType = S3.Audio
      let fileUrl =
            transporterConfig.mediaFileUrlPattern
              & T.replace "<DOMAIN>" "feedback"
              & T.replace "<FILE_PATH>" filePath
      mediaId <- createMediaEntry fileUrl fileType filePath
      return $ Just mediaId
    (_, _) -> return Nothing

createMediaEntry :: Text -> S3.FileType -> Text -> Flow (Id D.MediaFile)
createMediaEntry url fileType filePath = do
  fileEntity <- mkFile url
  _ <- QMF.create fileEntity
  return fileEntity.id
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        D.MediaFile
          { id,
            _type = fileType,
            url = fileUrl,
            s3FilePath = Just filePath,
            createdAt = now
          }
