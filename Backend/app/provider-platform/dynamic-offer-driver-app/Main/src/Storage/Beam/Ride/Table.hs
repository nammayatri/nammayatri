{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Ride.Table where

import qualified Database.Beam as B
import qualified Domain.Types.Ride as Domain
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data RideT f = RideT
  { id :: B.C f Text,
    bookingId :: B.C f Text,
    shortId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    status :: B.C f Domain.RideStatus,
    driverId :: B.C f Text,
    otp :: B.C f Text,
    trackingUrl :: B.C f Text,
    fare :: B.C f (Maybe Money),
    traveledDistance :: B.C f HighPrecMeters,
    chargeableDistance :: B.C f (Maybe Meters),
    driverArrivalTime :: B.C f (Maybe UTCTime),
    tripStartTime :: B.C f (Maybe UTCTime),
    tripEndTime :: B.C f (Maybe UTCTime),
    tripStartLat :: B.C f (Maybe Double),
    tripStartLon :: B.C f (Maybe Double),
    tripEndLat :: B.C f (Maybe Double),
    tripEndLon :: B.C f (Maybe Double),
    pickupDropOutsideOfThreshold :: B.C f (Maybe Bool),
    fareParametersId :: B.C f (Maybe Text),
    distanceCalculationFailed :: B.C f (Maybe Bool),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    driverDeviatedFromRoute :: B.C f (Maybe Bool),
    numberOfSnapToRoadCalls :: B.C f (Maybe Int),
    numberOfDeviation :: B.C f (Maybe Bool),
    uiDistanceCalculationWithAccuracy :: B.C f (Maybe Int),
    uiDistanceCalculationWithoutAccuracy :: B.C f (Maybe Int),
    driverGoHomeRequestId :: B.C f (Maybe Text),
    odometerStartReading :: B.C f (Maybe Meters),
    odometerEndReading :: B.C f (Maybe Meters),
    endRideOtp :: B.C f (Maybe Text),
    rideType :: B.C f Domain.RideType
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Ride = RideT Identity

$(enableKVPG ''RideT ['id] [['bookingId], ['driverId], ['driverGoHomeRequestId]])

$(mkTableInstances ''RideT "ride")
