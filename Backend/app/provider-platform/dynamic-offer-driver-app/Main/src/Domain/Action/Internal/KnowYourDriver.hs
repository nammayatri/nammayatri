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
import qualified Data.Text as T
import qualified Domain.Types.DocumentVerificationConfig as DTO
import Domain.Types.DriverModuleCompletion
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Person as DP
import Domain.Types.Ride
import qualified Domain.Types.Vehicle as DVeh
import Environment
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverModuleCompletion as SQDMC
import qualified Storage.Queries.DriverProfileQuestions as DPQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Feedback as QFeedback
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.LmsModule as QLmsModule
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RatingExtra as QRating
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

data DriverReview = DriverReview
  { review :: Maybe Text,
    feedBackPills :: [Text],
    rating :: Int,
    tripDate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

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
    vehicleNum :: Text,
    vechicleVariant :: DVeh.Variant,
    vehicleTags :: [Text],
    profileImage :: Maybe Text,
    images :: [Text],
    topReviews :: [DriverReview]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

knowYourDriver :: Id Ride -> Maybe Text -> Flow DriverProfileRes
knowYourDriver rideId apiKey =
  QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    >>= \ride ->
      getDriver ride.driverId apiKey
        >>= getDriverProfile

knowYourFavDriver :: Id DP.Person -> Maybe Text -> Flow DriverProfileRes
knowYourFavDriver driverId apiKey =
  getDriver driverId apiKey
    >>= getDriverProfile

getDriver :: Id DP.Person -> Maybe Text -> Flow DP.Person
getDriver driverId apiKey = do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound (driverId.getId))
  QM.findById (person.merchantId) >>= fromMaybeM (MerchantNotFound (person.merchantId.getId))
    >>= \merchant ->
      if Just (merchant.internalApiKey) == apiKey
        then pure person
        else throwError (AuthBlocked "Invalid BPP internal api key")

getDriverProfile :: DP.Person -> Flow DriverProfileRes
getDriverProfile person = do
  driverProfile <- DPQ.findByPersonId person.id
  driverStats <- B.runInReplica $ QDriverStats.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  vehicle <- QVeh.findById person.id >>= fromMaybeM (DriverWithoutVehicle person.id.getId)
  modules <- SQDMC.findByDriverIdAndStatus person.id MODULE_COMPLETED >>= mapM (\driverModule -> QLmsModule.findById driverModule.moduleId)
  images <- maybe (pure []) (maybe (pure []) (getImages . map Id) . (.imageIds)) driverProfile
  profileImage <- ImageQuery.findByPersonIdImageTypeAndValidationStatus (person.id) DTO.ProfilePhoto DImage.APPROVED >>= maybe (return Nothing) (\image -> S3.get (T.unpack (image.s3Path)) <&> Just)
  topFeedbacks <- getTopFeedBackForDriver person.id
  pure $
    DriverProfileRes
      { certificates = map show $ mapMaybe (fmap (.category)) modules,
        homeTown = Nothing,
        driverName = person.firstName,
        onboardedAt = person.createdAt,
        pledges = maybe [] (.pledges) driverProfile,
        languages = fromMaybe [] ((map getLanguages) <$> person.languagesSpoken),
        aboutMe = maybe Nothing (.aboutMe) driverProfile,
        drivingSince = maybe Nothing (.drivingSince) driverProfile,
        driverStats = getDriverStatsSummary driverStats,
        aspirations = fromMaybe [] (maybe Nothing (.aspirations) driverProfile),
        vehicleNum = vehicle.registrationNo,
        vechicleVariant = vehicle.variant,
        vehicleTags = fromMaybe [] (maybe Nothing (.vehicleTags) driverProfile),
        images = images,
        profileImage = profileImage,
        topReviews = topFeedbacks
      }
  where
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

    getImages imagePaths =
      mapM (QMF.findById) imagePaths
        >>= pure . catMaybes
        >>= pure . ((.url) <$>)
        >>= mapM (S3.get . T.unpack)

    getTopFeedBackForDriver driverId = do
      ratings <- QRating.findTopRatingsForDriver driverId (Just 5)
      partialRatings <- QFeedback.findFeedbackFromRatings (ratings <&> (.rideId)) >>= constructPartialRatings ratings
      remRatings <- constructRemRatings ratings
      pure $ partialRatings <> remRatings

    constructPartialRatings ratings feedbacks =
      pure $ foldl (goFeedbacks feedbacks) [] ratings

    goFeedbacks feedbacks fullRating rating =
      fullRating
        <> [ DriverReview
               { review = rating.feedbackDetails,
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

    constructRemRatings prevRatings = do
      feedbacks <- QFeedback.findOtherFeedbacks ((.rideId) <$> prevRatings) (Just 5)
      ratings <- (mapM QRating.findRatingForRideIfPositive ((.rideId) <$> feedbacks)) <&> catMaybes
      pure $ foldl (goFeedbacks feedbacks) [] ratings
