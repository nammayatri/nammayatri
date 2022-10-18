{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Ride where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Booking.Type as DRB
import Domain.Types.VehicleVariant (VehicleVariant)
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
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data BPPRide

data Ride = Ride
  { id :: Id Ride,
    bppRideId :: Id BPPRide,
    bookingId :: Id DRB.Booking,
    shortId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    driverMobileNumber :: Text,
    driverRegisteredAt :: UTCTime,
    vehicleNumber :: Text,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleVariant :: VehicleVariant,
    otp :: Text,
    trackingUrl :: Maybe BaseUrl,
    fare :: Maybe Money,
    totalFare :: Maybe Money,
    chargeableDistance :: Maybe HighPrecMeters,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverNumber :: Text,
    driverRatings :: Maybe Centesimal,
    driverRegisteredAt :: UTCTime,
    vehicleNumber :: Text,
    vehicleColor :: Text,
    vehicleVariant :: VehicleVariant,
    vehicleModel :: Text,
    rideOtp :: Text,
    computedPrice :: Maybe Money,
    chargeableRideDistance :: Maybe HighPrecMeters,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

makeRideAPIEntity :: Ride -> RideAPIEntity
makeRideAPIEntity Ride {..} =
  RideAPIEntity
    { shortRideId = shortId,
      driverNumber = driverMobileNumber,
      driverRatings = driverRating,
      rideOtp = otp,
      computedPrice = totalFare,
      chargeableRideDistance = chargeableDistance,
      ..
    }
