{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Ride where

import Beckn.Prelude (roundToIntegral)
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Beckn.Utils.Common
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Booking.Type as DRB
import qualified Domain.Types.Person as DPers
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Servant.API

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Ride = Ride
  { id :: Id Ride,
    bookingId :: Id DRB.Booking,
    shortId :: ShortId Ride,
    status :: RideStatus,
    driverId :: Id DPers.Person,
    otp :: Text,
    trackingUrl :: BaseUrl,
    fare :: Maybe Money,
    totalFare :: Maybe Money,
    traveledDistance :: HighPrecMeters,
    chargeableDistance :: Maybe Meters,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    tripStartPos :: Maybe LatLong,
    tripEndPos :: Maybe LatLong,
    rideRating :: Maybe RideRating,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data RideRating = RideRating
  { id :: Id RideRating,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: DVeh.Variant,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Money,
    computedTotalFare :: Maybe Money,
    actualRideDistance :: Meters,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    chargeableDistance :: Maybe Meters
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

makeRideAPIEntity :: Ride -> DPers.DecryptedPerson -> DVeh.Vehicle -> RideAPIEntity
makeRideAPIEntity ride driver vehicle =
  RideAPIEntity
    { id = ride.id,
      shortRideId = ride.shortId,
      status = ride.status,
      driverName = driver.firstName,
      driverNumber = driver.mobileCountryCode <> driver.mobileNumber,
      vehicleNumber = vehicle.registrationNo,
      vehicleColor = vehicle.color,
      vehicleVariant = vehicle.variant,
      vehicleModel = vehicle.model,
      computedFare = ride.fare,
      computedTotalFare = ride.totalFare,
      actualRideDistance = roundToIntegral ride.traveledDistance,
      rideRating = ride.rideRating <&> (.ratingValue),
      createdAt = ride.createdAt,
      updatedAt = ride.updatedAt,
      chargeableDistance = ride.chargeableDistance
    }
