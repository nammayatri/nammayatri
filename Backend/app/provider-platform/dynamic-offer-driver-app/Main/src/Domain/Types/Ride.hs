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
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Booking as DRB
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest (DriverGoHomeRequest)
import qualified Domain.Types.FareParameters as DFare
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPers
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types
import qualified Kernel.Prelude as BP
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, BP.ToParamSchema)

$(mkBeamInstancesForEnum ''RideStatus)

$(mkHttpInstancesForEnum ''RideStatus)

data Ride = Ride
  { id :: Id Ride,
    bookingId :: Id DRB.Booking,
    shortId :: ShortId Ride,
    merchantId :: Maybe (Id DM.Merchant),
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    status :: RideStatus,
    driverId :: Id DPers.Person,
    otp :: Text,
    trackingUrl :: BaseUrl,
    fare :: Maybe Money,
    traveledDistance :: HighPrecMeters,
    chargeableDistance :: Maybe Meters,
    driverArrivalTime :: Maybe UTCTime,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    tripStartPos :: Maybe LatLong,
    tripEndPos :: Maybe LatLong,
    fromLocation :: DL.Location,
    toLocation :: Maybe DL.Location,
    fareParametersId :: Maybe (Id DFare.FareParameters),
    distanceCalculationFailed :: Maybe Bool,
    pickupDropOutsideOfThreshold :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    driverDeviatedFromRoute :: Maybe Bool,
    numberOfSnapToRoadCalls :: Maybe Int,
    numberOfOsrmSnapToRoadCalls :: Maybe Int,
    numberOfDeviation :: Maybe Bool,
    uiDistanceCalculationWithAccuracy :: Maybe Int,
    uiDistanceCalculationWithoutAccuracy :: Maybe Int,
    isFreeRide :: Maybe Bool,
    driverGoHomeRequestId :: Maybe (Id DriverGoHomeRequest),
    safetyAlertTriggered :: Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
