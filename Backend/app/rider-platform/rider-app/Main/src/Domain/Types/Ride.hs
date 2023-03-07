{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Ride where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.Booking.Type as DRB
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
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
    driverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

data ShareRideInfo = ShareRideInfo
  { id :: Id Ride,
    bookingId :: Id DRB.Booking,
    status :: RideStatus,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    driverMobileNumber :: Text,
    vehicleNumber :: Text,
    vehicleModel :: Text,
    vehicleColor :: Text,
    trackingUrl :: Maybe BaseUrl,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    userFirstName :: Maybe Text,
    userLastName :: Maybe Text,
    fromLocation :: DLoc.BookingLocation,
    toLocation :: Maybe DLoc.BookingLocation
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

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
    driverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    bppRideId :: Id BPPRide
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
