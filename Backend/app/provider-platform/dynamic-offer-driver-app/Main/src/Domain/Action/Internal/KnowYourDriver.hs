{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.KnowYourDriver where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.DocumentVerificationConfig as DTO
import Domain.Types.DriverModuleCompletion
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import Domain.Types.Ride
import qualified Domain.Types.VehicleVariant as DVeh
import Environment
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error
import Lib.Yudhishthira.Tools.Utils (accessTagKey, convertTags)
import Lib.Yudhishthira.Types (TagName (..))
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverModuleCompletion as SQDMC
import qualified Storage.Queries.DriverProfileQuestions as DPQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Feedback as QFeedback
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.LmsModule as QLmsModule
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.BookingLite as QBLite
import qualified Storage.Queries.QueriesExtra.RideLite as QRLite
import qualified Storage.Queries.RatingExtra as QRating
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

data DriverReview = DriverReview
  { riderName :: Maybe Text,
    review :: Maybe Text,
    feedBackPills :: [Text],
    rating :: Int,
    tripDate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, Eq)

data DriverStatSummary = DriverStatSummary
  { avgRating :: Maybe Centesimal,
    numTrips :: Int,
    cancellationRate :: Int,
    likedByRidersNum :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DriverProfileRes = DriverProfileRes
  { certificates :: [Text],
    homeTown :: Maybe Text,
    driverName :: Text,
    aboutMe :: Maybe Text,
    drivingSince :: Maybe Int,
    onboardedAt :: UTCTime,
    pledges :: [Text],
    driverStats :: DriverStatSummary,
    languages :: [Text],
    aspirations :: [Text],
    vehicleNum :: Maybe Text,
    vechicleVariant :: Maybe DVeh.VehicleVariant,
    vehicleTags :: [Text],
    profileImage :: Maybe Text,
    images :: [Text],
    topReviews :: [DriverReview],
    driverSafetyScore :: Maybe A.Value,
    driverTags :: Maybe A.Value
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

knowYourDriver :: Id Ride -> Maybe Bool -> Maybe Text -> Flow DriverProfileRes
knowYourDriver rideId withImages apiKey = do
  let rideLiteId = cast rideId :: Id QRLite.RideLite
  QRLite.findByIdLite rideLiteId >>= fromMaybeM (RideNotFound rideId.getId)
    >>= \ride ->
      getDriver ride.driverId apiKey
        >>= getDriverProfile withImages

knowYourFavDriver :: Id DP.Person -> Maybe Bool -> Maybe Text -> Flow DriverProfileRes
knowYourFavDriver driverId withImages apiKey =
  getDriver driverId apiKey
    >>= getDriverProfile withImages

getDriver :: Id DP.Person -> Maybe Text -> Flow DP.Person
getDriver driverId apiKey = do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound (driverId.getId))
  QM.findById (person.merchantId) >>= fromMaybeM (MerchantNotFound (person.merchantId.getId))
    >>= \merchant ->
      if Just (merchant.internalApiKey) == apiKey
        then pure person
        else throwError (AuthBlocked "Invalid BPP internal api key")

getDriverProfile :: Maybe Bool -> DP.Person -> Flow DriverProfileRes
getDriverProfile withImages person = do
  driverProfile <- DPQ.findByPersonId person.id
  driverStats <- B.runInReplica $ QDriverStats.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  vehicle <- QVeh.findById person.id
  modules <- SQDMC.findByDriverIdAndStatus person.id MODULE_COMPLETED >>= mapM (\driverModule -> QLmsModule.findById driverModule.moduleId)
  images <- if withImages == Just True then maybe (pure []) (maybe (pure []) (getImages . map Id) . (.imageIds)) driverProfile else pure []
  profileImage <- case person.faceImageId of
    Just mediaId -> do
      mediaEntry <- B.runInReplica $ QMF.findById mediaId >>= fromMaybeM (FileDoNotExist person.id.getId)
      case mediaEntry.s3FilePath of
        Just s3Path -> Just <$> S3.get (T.unpack s3Path)
        _ -> fetchLegacyProfileImage person.id
    Nothing -> do
      fetchLegacyProfileImage person.id
  topFeedbacks <- getTopFeedBackForDriver person.id
  let driverTagsJson = convertTags $ fold person.driverTag
  let mbSafetyScoreFromTag = accessTagKey (TagName "SafetyScore") driverTagsJson
  let mbSafetyCohort = accessTagKey (TagName "SafetyCohort") driverTagsJson
  let driverSafetyScore = mbSafetyScoreFromTag <|> getSafetyScoreFromSafetyCohort mbSafetyCohort
  pure $
    DriverProfileRes
      { certificates = map show $ mapMaybe (fmap (.category)) modules,
        homeTown = Nothing,
        driverName = person.firstName,
        onboardedAt = person.createdAt,
        pledges = foldMap (.pledges) driverProfile,
        languages = fold ((map getLanguages) <$> person.languagesSpoken),
        aboutMe = (.aboutMe) =<< driverProfile,
        drivingSince = (.drivingSince) =<< driverProfile,
        driverStats = getDriverStatsSummary driverStats,
        aspirations = fold ((.aspirations) =<< driverProfile),
        vehicleNum = (.registrationNo) <$> vehicle,
        vechicleVariant = (.variant) <$> vehicle,
        vehicleTags = fold ((.vehicleTags) =<< driverProfile),
        images = images,
        profileImage = profileImage,
        topReviews = topFeedbacks,
        driverSafetyScore = driverSafetyScore,
        driverTags = Just driverTagsJson
      }
  where
    fetchLegacyProfileImage personId =
      ImageQuery.findByPersonIdImageTypeAndValidationStatus personId DTO.ProfilePhoto DImage.APPROVED >>= maybe (return Nothing) (\image -> S3.get (T.unpack (image.s3Path)) <&> Just)

    nonZero Nothing = 1
    nonZero (Just a)
      | a <= 0 = 1
      | otherwise = a

    getDriverStatsSummary driverStats =
      DriverStatSummary
        { avgRating = driverStats.rating,
          numTrips = driverStats.totalRides,
          cancellationRate = div ((fromMaybe 0 driverStats.ridesCancelled) * 100 :: Int) (nonZero driverStats.totalRidesAssigned :: Int),
          likedByRidersNum = driverStats.favRiderCount
        }

    getImages imagePaths = do
      mapM (QMF.findById) imagePaths
        >>= pure . catMaybes
        >>= pure . ((.url) <$>)
        >>= mapM (S3.get . T.unpack . extractFilePath)

    getTopFeedBackForDriver driverId = do
      ratings <- QRating.findTopRatingsForDriver driverId (Just 5)
      ratingsWithDriverNames <- getBookingsAndRides ratings
      partialRatings <- QFeedback.findFeedbackFromRatings (ratings <&> (.rideId)) >>= constructPartialRatings ratingsWithDriverNames
      remRatings <- constructRemRatings ratingsWithDriverNames driverId
      pure $ nub $ partialRatings <> remRatings

    constructPartialRatings ratings feedbacks =
      pure $ foldl (goFeedbacks feedbacks) [] ratings

    goFeedbacks feedbacks fullRating (rating, riderName) =
      fullRating
        <> [ DriverReview
               { riderName = riderName,
                 review = rating.feedbackDetails,
                 feedBackPills = filter (\feedback -> feedback.rideId == rating.rideId) feedbacks <&> (.badge), -- map () ratingFeedbacks,
                 rating = rating.ratingValue,
                 tripDate = rating.createdAt
               }
           ]

    getLanguages = \case
      "EN_US" -> "English"
      "HI_IN" -> "Hindi"
      "TA_IN" -> "Tamil"
      "KN_IN" -> "Kannada"
      "TE_IN" -> "Telugu"
      "BN_IN" -> "Bengali"
      "ML_IN" -> "Malayalam"
      lang -> lang

    constructRemRatings prevRatings driverId = do
      feedbacks <- QFeedback.findOtherFeedbacks ((.rideId) <$> (fst <$> prevRatings)) driverId (Just 10)
      ratings <- (QRating.findRatingForRideIfPositive (nub ((.rideId) <$> feedbacks)))
      ratingsWithDriverNames <- getBookingsAndRides ratings
      pure $ take 5 $ foldl (goFeedbacks feedbacks) [] ratingsWithDriverNames

    extractFilePath url = case T.splitOn "filePath=" url of
      [_before, after] -> after
      _ -> T.empty

    getBookingsAndRides :: [DRating.Rating] -> Flow [(DRating.Rating, Maybe Text)]
    getBookingsAndRides ratings = do
      let rideLiteIds = cast . (.rideId) <$> ratings
      rides <- QRLite.findRidesFromDBLite rideLiteIds
      let bookingLiteIds = cast . (.bookingId) <$> rides
      bookings <- QBLite.findBookingsFromDBLite bookingLiteIds
      let ratingBookingIds = constructRatingWithBookingId ratings rides
      pure (constructRatingWithRiderName bookings ratingBookingIds)

    constructRatingWithBookingId ratings rides =
      foldl (mkRatingWithBookingId rides) [] ratings

    constructRatingWithRiderName bookings ratings =
      foldl (mkRatingWithRiderName bookings) [] ratings

    mkRatingWithBookingId rides ratingWithBooking rating =
      ratingWithBooking
        <> [(rating, fromMaybe "" $ find (\ride -> ride.id == rating.rideId) rides <&> (.bookingId))]

    mkRatingWithRiderName bookings ratingWithBooking (rating, bookingId) =
      ratingWithBooking
        <> [(rating, fromMaybe Nothing $ find (\booking -> booking.id == bookingId) bookings <&> (.riderName))]

    getSafetyScoreFromSafetyCohort mbSafetyCohort = do
      case mbSafetyCohort of
        Just "Safe" -> Just (A.Number 0.0)
        Just "Problematic" -> Just (A.Number 0.5)
        Just "Unsafe" -> Just (A.Number 1.0)
        _ -> Just (A.Number 0.25)
