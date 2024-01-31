{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Ride where

import Data.Aeson
import qualified Domain.Types.Booking.Type as DRB
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''RideStatus)

$(mkHttpInstancesForEnum ''RideStatus)

data BPPRide

data Ride = Ride
  { id :: Id Ride,
    bppRideId :: Id BPPRide,
    bookingId :: Id DRB.Booking,
    shortId :: ShortId Ride,
    merchantId :: Maybe (Id DM.Merchant),
    fromLocation :: DL.Location,
    toLocation :: Maybe DL.Location,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    status :: RideStatus,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Maybe Text,
    driverRegisteredAt :: UTCTime,
    driverImage :: Maybe Text,
    vehicleNumber :: Text,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleVariant :: VehicleVariant,
    otp :: Text,
    endOtp :: Maybe Text,
    trackingUrl :: Maybe BaseUrl,
    fare :: Maybe Money,
    totalFare :: Maybe Money,
    chargeableDistance :: Maybe HighPrecMeters,
    traveledDistance :: Maybe HighPrecMeters,
    driverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideRating :: Maybe Int,
    allowedEditLocationAttempts :: Maybe Int,
    isFreeRide :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    safetyCheckStatus :: Maybe Bool
  }
  deriving (Generic, Show)

data RideAPIEntity = RideAPIEntity
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    driverName :: Text,
    driverNumber :: Maybe Text,
    driverRatings :: Maybe Centesimal,
    driverRegisteredAt :: UTCTime,
    driverImage :: Maybe Text,
    vehicleNumber :: Text,
    vehicleColor :: Text,
    vehicleVariant :: VehicleVariant,
    vehicleModel :: Text,
    rideOtp :: Text,
    endOtp :: Maybe Text,
    computedPrice :: Maybe Money,
    chargeableRideDistance :: Maybe HighPrecMeters,
    driverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideRating :: Maybe Int,
    isFreeRide :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    bppRideId :: Id BPPRide
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

makeRideAPIEntity :: Ride -> RideAPIEntity
makeRideAPIEntity Ride {..} =
  let driverMobileNumber' = if status == NEW then Just driverMobileNumber else Just "xxxx"
   in RideAPIEntity
        { shortRideId = shortId,
          driverNumber = driverMobileNumber',
          driverRatings = driverRating,
          rideOtp = otp,
          computedPrice = totalFare,
          chargeableRideDistance = chargeableDistance,
          ..
        }
