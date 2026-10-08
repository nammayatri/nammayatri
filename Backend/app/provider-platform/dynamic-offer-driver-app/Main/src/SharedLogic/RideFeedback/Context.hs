module SharedLogic.RideFeedback.Context
  ( RideFeedbackContext (..),
    BookingCtx (..),
    RideCtx (..),
    DriverCtx (..),
    CityCtx (..),
    DerivedCtx (..),
    AnswerCtx (..),
    ContextInput (..),
    buildRideFeedbackContext,
    withAnswer,
    mkAnswerCtx,
  )
where

import Data.Default.Class (Default (..))
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.Booking as DB
import Domain.Types.Common (ServiceTierType (COMFY), TripCategory, isRideOtpTrip)
import qualified Domain.Types.DriverStats as DDS
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackConfig as DRFC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import Domain.Types.VehicleVariant (VehicleVariant (SEDAN))
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Seconds (..))
import Kernel.Types.Id
import SharedLogic.RideFeedback.Rule (stableBucket)

-- | The JSON object the RIDE-FEEDBACK targeting logic and action rules are evaluated against.
-- Fields are listed explicitly (no whole-entity serialisation) so OTPs, phone numbers
-- and other PII never reach a rule or the dashboard preview. ride and driver are null before a
-- driver is assigned (booking stages).
data RideFeedbackContext = RideFeedbackContext
  { stage :: DRFC.RideFeedbackStage,
    booking :: BookingCtx,
    ride :: Maybe RideCtx,
    driver :: Maybe DriverCtx,
    city :: CityCtx,
    derived :: DerivedCtx,
    answer :: Maybe AnswerCtx
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingCtx = BookingCtx
  { merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    vehicleServiceTierType :: ServiceTierType,
    serviceTierName :: Text,
    tripCategory :: Text,
    -- | The rider shows an OTP to a driver at pickup (airport and spot queues); the ride starts then.
    isRideOtp :: Bool,
    isAirConditioned :: Maybe Bool,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    estimatedFare :: Double,
    estimatedDistanceMeters :: Maybe Int,
    estimatedDurationSeconds :: Maybe Int,
    isScheduled :: Bool,
    isPetRide :: Bool,
    hasStops :: Maybe Bool,
    roundTrip :: Maybe Bool,
    -- | The rider platform (BAP) that booked the ride.
    bapId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideCtx = RideCtx
  { status :: DRide.RideStatus,
    vehicleVariant :: Maybe VehicleVariant,
    rideTags :: [Text],
    traveledDistanceMeters :: Double,
    isAirConditioned :: Maybe Bool,
    isInsured :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | The driver of the ride: what driver-side targeting can use.
data DriverCtx = DriverCtx
  { driverTags :: [Text],
    rating :: Maybe Double,
    totalRides :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CityCtx = CityCtx
  { -- | The city name, e.g. "Chennai" (City's own JSON is its STD code, e.g. "std:044").
    cityName :: Text,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DerivedCtx = DerivedCtx
  { -- | Since the journey was first booked (a reallocation keeps counting from the first booking).
    secondsSinceBooking :: Int,
    secondsSinceRideStart :: Maybe Int,
    secondsSinceAssign :: Maybe Int,
    localHour :: Int,
    dayOfWeek :: Text,
    -- | Stable 0–99 bucket of the rider (or the ride when the rider is unknown), for % splits in rules.
    rolloutBucket :: Int
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

-- | Sample input the dashboard shows when verifying a RIDE_FEEDBACK logic (a Bangalore AC cab, 5 minutes into the ride).
instance Default RideFeedbackContext where
  def =
    RideFeedbackContext
      { stage = DRFC.RIDE_IN_PROGRESS,
        booking =
          BookingCtx
            { merchantOperatingCityId = Id "merchant-operating-city-id",
              vehicleServiceTierType = COMFY,
              serviceTierName = "AC Mini",
              tripCategory = "OneWay",
              isRideOtp = False,
              isAirConditioned = Just True,
              specialLocationTag = Nothing,
              specialLocationName = Nothing,
              estimatedFare = 310,
              estimatedDistanceMeters = Just 8000,
              estimatedDurationSeconds = Just 1200,
              isScheduled = False,
              isPetRide = False,
              hasStops = Just False,
              roundTrip = Just False,
              bapId = "bap-id"
            },
        ride =
          Just
            RideCtx
              { status = DRide.INPROGRESS,
                vehicleVariant = Just SEDAN,
                rideTags = [],
                traveledDistanceMeters = 2000,
                isAirConditioned = Just True,
                isInsured = False
              },
        driver = Just DriverCtx {driverTags = [], rating = Just 4.8, totalRides = 250},
        city = CityCtx {cityName = "Bangalore", merchantOperatingCityId = Id "merchant-operating-city-id"},
        derived =
          DerivedCtx
            { secondsSinceBooking = 900,
              secondsSinceRideStart = Just 300,
              secondsSinceAssign = Just 600,
              localHour = 18,
              dayOfWeek = "MON",
              rolloutBucket = 42
            },
        answer = Nothing
      }

data ContextInput = ContextInput
  { now :: UTCTime,
    timeDiffFromUtc :: Seconds,
    stage :: DRFC.RideFeedbackStage,
    booking :: DB.Booking,
    -- | When the journey was first booked.
    bookedAt :: UTCTime,
    -- | The ride with its driver and their stats, once a driver is assigned.
    rideWithDriver :: Maybe (DRide.Ride, DP.Person, Maybe DDS.DriverStats),
    city :: Context.City
  }

buildRideFeedbackContext :: ContextInput -> RideFeedbackContext
buildRideFeedbackContext ContextInput {..} =
  RideFeedbackContext
    { stage,
      booking =
        BookingCtx
          { merchantOperatingCityId = booking.merchantOperatingCityId,
            vehicleServiceTierType = booking.vehicleServiceTier,
            serviceTierName = booking.vehicleServiceTierName,
            tripCategory = tripCategoryTag booking.tripCategory,
            isRideOtp = isRideOtpTrip booking.tripCategory,
            isAirConditioned = booking.isAirConditioned,
            specialLocationTag = booking.specialLocationTag,
            specialLocationName = booking.specialLocationName,
            estimatedFare = realToFrac booking.estimatedFare,
            estimatedDistanceMeters = (.getMeters) <$> booking.estimatedDistance,
            estimatedDurationSeconds = (.getSeconds) <$> booking.estimatedDuration,
            isScheduled = booking.isScheduled,
            isPetRide = booking.isPetRide,
            hasStops = booking.hasStops,
            roundTrip = booking.roundTrip,
            bapId = booking.bapId
          },
      ride =
        mbRide <&> \ride ->
          RideCtx
            { status = ride.status,
              vehicleVariant = ride.vehicleVariant,
              rideTags = map (tagName . (.getTagNameValue)) (fromMaybe [] ride.rideTags),
              traveledDistanceMeters = realToFrac ride.traveledDistance,
              isAirConditioned = ride.isAirConditioned,
              isInsured = ride.isInsured
            },
      driver =
        rideWithDriver <&> \(_, driverPerson, driverStats) ->
          DriverCtx
            { driverTags = map (tagName . (.getTagNameValueExpiry)) (fromMaybe [] driverPerson.driverTag),
              rating = realToFrac <$> (driverStats >>= (.rating)),
              totalRides = maybe 0 (.totalRides) driverStats
            },
      city = CityCtx {cityName = show city, merchantOperatingCityId = booking.merchantOperatingCityId},
      derived =
        DerivedCtx
          { secondsSinceBooking = secondsSince bookedAt,
            secondsSinceRideStart = secondsSince <$> (mbRide >>= (.tripStartTime)),
            secondsSinceAssign = secondsSince . (.createdAt) <$> mbRide,
            localHour = Time.todHour localTime,
            dayOfWeek = T.toUpper . T.take 3 . show $ Time.dayOfWeek (Time.localDay localDateTime),
            rolloutBucket = stableBucket (maybe booking.id.getId (.getId) booking.riderId)
          },
      answer = Nothing
    }
  where
    mbRide = rideWithDriver <&> \(ride, _, _) -> ride
    secondsSince t = floor (Time.diffUTCTime now t)
    localDateTime = Time.utcToLocalTime Time.utc (Time.addUTCTime (fromIntegral timeDiffFromUtc.getSeconds) now)
    localTime = Time.localTimeOfDay localDateTime
    -- Tags are stored as name#value(#expiry); rules match on the name.
    tagName = T.takeWhile (/= '#')

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
