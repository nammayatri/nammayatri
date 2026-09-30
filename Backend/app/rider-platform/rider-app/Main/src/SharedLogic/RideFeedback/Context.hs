module SharedLogic.RideFeedback.Context
  ( RideFeedbackContext (..),
    BookingCtx (..),
    RideCtx (..),
    PersonCtx (..),
    CityCtx (..),
    DerivedCtx (..),
    AnswerCtx (..),
    ContextInput (..),
    buildRideFeedbackContext,
    withAnswer,
    mkAnswerCtx,
    rideProgressPct,
  )
where

import BecknV2.OnDemand.Enums (VehicleCategory (CAB))
import Control.Applicative ((<|>))
import Data.Default.Class (Default (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.Booking as DB
import Domain.Types.Common (TripCategory)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackResponse as DRFR
import qualified Domain.Types.RideStatus as DRS
import Domain.Types.ServiceTierType (ServiceTierType (COMFY))
import Domain.Types.VehicleVariant (VehicleVariant (SEDAN))
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Seconds (..), distanceToHighPrecMeters)
import Kernel.Types.Id
import SharedLogic.RideFeedback.Rule (stableBucket)

-- | The JSON object eligibility and action rules are evaluated against.
-- Fields are listed explicitly (no whole-entity serialisation) so OTPs, phone numbers
-- and other PII never reach a rule or the dashboard preview.
data RideFeedbackContext = RideFeedbackContext
  { booking :: BookingCtx,
    ride :: RideCtx,
    person :: PersonCtx,
    city :: CityCtx,
    derived :: DerivedCtx,
    answer :: Maybe AnswerCtx
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingCtx = BookingCtx
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    vehicleServiceTierType :: ServiceTierType,
    vehicleCategory :: Maybe VehicleCategory,
    tripCategory :: Maybe Text,
    isAirConditioned :: Maybe Bool,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    serviceTierName :: Maybe Text,
    estimatedFare :: Double,
    estimatedDistanceMeters :: Maybe Double,
    estimatedDurationSeconds :: Maybe Int,
    isScheduled :: Bool,
    isPetRide :: Bool,
    hasStops :: Maybe Bool,
    roundTrip :: Maybe Bool,
    providerId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideCtx = RideCtx
  { status :: DRS.RideStatus,
    vehicleVariant :: VehicleVariant,
    rideTags :: [Text],
    driverRating :: Maybe Double,
    traveledDistanceMeters :: Maybe Double,
    isSafetyPlus :: Bool,
    isInsured :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PersonCtx = PersonCtx
  { language :: Maybe Language,
    totalRatings :: Int,
    totalRidesCount :: Maybe Int,
    hasDisability :: Maybe Bool,
    customerTags :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CityCtx = CityCtx
  { -- | The city name, e.g. "Chennai" (City's own JSON is its STD code, e.g. "std:044").
    cityName :: Text,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DerivedCtx = DerivedCtx
  { secondsSinceRideStart :: Maybe Int,
    secondsSinceAssign :: Int,
    distanceCoveredPct :: Maybe Int,
    localHour :: Int,
    dayOfWeek :: Text,
    rolloutBucket :: Int,
    shownCount :: HM.HashMap Text Int,
    daysSinceLastAsked :: HM.HashMap Text Int,
    rideEvents :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AnswerCtx = AnswerCtx
  { selectedOptionKeys :: [Text],
    primaryOptionKey :: Maybe Text,
    text :: Maybe Text,
    number :: Maybe Double,
    rating :: Maybe Int,
    hasMedia :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Sample input the dashboard shows when verifying a RIDE-FEEDBACK logic (a Bangalore AC cab, 5 minutes into the ride).
instance Default RideFeedbackContext where
  def =
    RideFeedbackContext
      { booking =
          BookingCtx
            { merchantId = Id "merchant-id",
              merchantOperatingCityId = Id "merchant-operating-city-id",
              vehicleServiceTierType = COMFY,
              vehicleCategory = Just CAB,
              tripCategory = Just "OneWay",
              isAirConditioned = Just True,
              specialLocationTag = Nothing,
              specialLocationName = Nothing,
              serviceTierName = Just "AC Mini",
              estimatedFare = 310,
              estimatedDistanceMeters = Just 8000,
              estimatedDurationSeconds = Just 1200,
              isScheduled = False,
              isPetRide = False,
              hasStops = Just False,
              roundTrip = Just False,
              providerId = "provider-id"
            },
        ride =
          RideCtx
            { status = DRS.INPROGRESS,
              vehicleVariant = SEDAN,
              rideTags = [],
              driverRating = Just 4.8,
              traveledDistanceMeters = Nothing,
              isSafetyPlus = False,
              isInsured = False
            },
        person = PersonCtx {language = Nothing, totalRatings = 0, totalRidesCount = Just 10, hasDisability = Nothing, customerTags = []},
        city = CityCtx {cityName = "Bangalore", merchantOperatingCityId = Id "merchant-operating-city-id"},
        derived =
          DerivedCtx
            { secondsSinceRideStart = Just 300,
              secondsSinceAssign = 600,
              distanceCoveredPct = Just 25,
              localHour = 18,
              dayOfWeek = "MON",
              rolloutBucket = 42,
              shownCount = HM.empty,
              daysSinceLastAsked = HM.empty,
              rideEvents = []
            },
        answer = Nothing
      }

data ContextInput = ContextInput
  { now :: UTCTime,
    timeDiffFromUtc :: Seconds,
    booking :: DB.Booking,
    ride :: DRide.Ride,
    person :: DP.Person,
    city :: Context.City,
    rideResponses :: [DRFR.RideFeedbackResponse],
    -- | The rider's responses from other rides within the longest configured cooldown.
    historyResponses :: [DRFR.RideFeedbackResponse],
    rideEvents :: [Text]
  }

buildRideFeedbackContext :: ContextInput -> RideFeedbackContext
buildRideFeedbackContext ContextInput {..} =
  RideFeedbackContext
    { booking =
        BookingCtx
          { merchantId = booking.merchantId,
            merchantOperatingCityId = booking.merchantOperatingCityId,
            vehicleServiceTierType = booking.vehicleServiceTierType,
            vehicleCategory = booking.vehicleCategory,
            tripCategory = tripCategoryTag <$> booking.tripCategory,
            isAirConditioned = booking.isAirConditioned,
            specialLocationTag = booking.specialLocationTag,
            specialLocationName = booking.specialLocationName,
            serviceTierName = booking.serviceTierName,
            estimatedFare = realToFrac booking.estimatedFare.amount,
            estimatedDistanceMeters = realToFrac . distanceToHighPrecMeters <$> booking.estimatedDistance,
            estimatedDurationSeconds = (.getSeconds) <$> booking.estimatedDuration,
            isScheduled = booking.isScheduled,
            isPetRide = booking.isPetRide,
            hasStops = booking.hasStops,
            roundTrip = booking.roundTrip,
            providerId = booking.providerId
          },
      ride =
        RideCtx
          { status = ride.status,
            vehicleVariant = ride.vehicleVariant,
            rideTags = fromMaybe [] ride.rideTags,
            driverRating = realToFrac <$> ride.driverRating,
            traveledDistanceMeters = realToFrac . distanceToHighPrecMeters <$> ride.traveledDistance,
            isSafetyPlus = ride.isSafetyPlus,
            isInsured = ride.isInsured
          },
      person =
        PersonCtx
          { language = person.language,
            totalRatings = person.totalRatings,
            totalRidesCount = person.totalRidesCount,
            hasDisability = person.hasDisability,
            customerTags = map (T.takeWhile (/= '#') . (.getTagNameValueExpiry)) (fromMaybe [] person.customerNammaTags)
          },
      city = CityCtx {cityName = show city, merchantOperatingCityId = booking.merchantOperatingCityId},
      derived =
        DerivedCtx
          { secondsSinceRideStart = secondsSince <$> ride.rideStartTime,
            secondsSinceAssign = secondsSince ride.createdAt,
            distanceCoveredPct = rideProgressPct now booking ride,
            localHour = Time.todHour localTime,
            dayOfWeek = T.toUpper . T.take 3 . show $ Time.dayOfWeek (Time.localDay localDateTime),
            rolloutBucket = stableBucket person.id.getId,
            shownCount = HM.fromListWith (+) [(r.questionKey, r.shownCount) | r <- rideResponses],
            daysSinceLastAsked = HM.fromListWith min [(r.questionKey, daysSince r.createdAt) | r <- historyResponses],
            rideEvents
          },
      answer = Nothing
    }
  where
    secondsSince t = floor (Time.diffUTCTime now t)
    daysSince t = secondsSince t `div` 86400
    localDateTime = Time.utcToLocalTime Time.utc (Time.addUTCTime (fromIntegral timeDiffFromUtc.getSeconds) now)
    localTime = Time.localTimeOfDay localDateTime

-- | Ride progress in percent: travelled distance when the BPP has sent it, otherwise
-- time since start against the estimated duration (the rider side rarely has live distance).
rideProgressPct :: UTCTime -> DB.Booking -> DRide.Ride -> Maybe Int
rideProgressPct now booking ride =
  byDistance <|> byTime
  where
    byDistance = do
      travelled <- realToFrac . distanceToHighPrecMeters <$> ride.traveledDistance
      estimated <- realToFrac . distanceToHighPrecMeters <$> booking.estimatedDistance
      guard (estimated > (0 :: Double))
      pure . min 100 $ floor (travelled * 100 / estimated)
    byTime = do
      started <- ride.rideStartTime
      duration <- (.getSeconds) <$> booking.estimatedDuration
      guard (duration > 0)
      let elapsed = floor (Time.diffUTCTime now started) :: Int
      pure . min 100 $ elapsed * 100 `div` duration

withAnswer :: DRFR.RideFeedbackAnswer -> RideFeedbackContext -> RideFeedbackContext
withAnswer ans ctx = ctx {answer = Just (mkAnswerCtx ans)}

mkAnswerCtx :: DRFR.RideFeedbackAnswer -> AnswerCtx
mkAnswerCtx ans =
  let keys = fromMaybe [] ans.selectedOptionKeys
   in AnswerCtx
        { selectedOptionKeys = keys,
          primaryOptionKey = listToMaybe keys,
          text = ans.text,
          number = ans.number,
          rating = ans.rating,
          hasMedia = maybe False (not . null) ans.mediaFileIds
        }

-- | Constructor name only ("OneWay", "Rental", "InterCity", ...), which is what rules compare against.
tripCategoryTag :: TripCategory -> Text
tripCategoryTag = T.takeWhile (/= ' ') . show
