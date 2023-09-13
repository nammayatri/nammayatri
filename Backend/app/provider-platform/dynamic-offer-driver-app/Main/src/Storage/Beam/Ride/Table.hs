{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Ride.Table where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Ride as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.RideStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.RideStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.RideStatus

instance FromBackendRow Postgres Domain.RideStatus

instance IsString Domain.RideStatus where
  fromString = show

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
    driverArrivalTime :: B.C f (Maybe Time.UTCTime),
    tripStartTime :: B.C f (Maybe Time.UTCTime),
    tripEndTime :: B.C f (Maybe Time.UTCTime),
    tripStartLat :: B.C f (Maybe Double),
    tripStartLon :: B.C f (Maybe Double),
    tripEndLat :: B.C f (Maybe Double),
    tripEndLon :: B.C f (Maybe Double),
    pickupDropOutsideOfThreshold :: B.C f (Maybe Bool),
    fareParametersId :: B.C f (Maybe Text),
    distanceCalculationFailed :: B.C f (Maybe Bool),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    driverDeviatedFromRoute :: B.C f (Maybe Bool),
    numberOfSnapToRoadCalls :: B.C f (Maybe Int),
    numberOfDeviation :: B.C f (Maybe Bool),
    uiDistanceCalculationWithAccuracy :: B.C f (Maybe Int),
    uiDistanceCalculationWithoutAccuracy :: B.C f (Maybe Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Ride = RideT Identity

rideTMod :: RideT (B.FieldModification (B.TableField RideT))
rideTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      bookingId = B.fieldNamed "booking_id",
      shortId = B.fieldNamed "short_id",
      merchantId = B.fieldNamed "merchant_id",
      status = B.fieldNamed "status",
      driverId = B.fieldNamed "driver_id",
      otp = B.fieldNamed "otp",
      trackingUrl = B.fieldNamed "tracking_url",
      fare = B.fieldNamed "fare",
      traveledDistance = B.fieldNamed "traveled_distance",
      chargeableDistance = B.fieldNamed "chargeable_distance",
      driverArrivalTime = B.fieldNamed "driver_arrival_time",
      tripStartTime = B.fieldNamed "trip_start_time",
      tripEndTime = B.fieldNamed "trip_end_time",
      tripStartLat = B.fieldNamed "trip_start_lat",
      tripStartLon = B.fieldNamed "trip_start_lon",
      tripEndLat = B.fieldNamed "trip_end_lat",
      tripEndLon = B.fieldNamed "trip_end_lon",
      pickupDropOutsideOfThreshold = B.fieldNamed "pickup_drop_outside_of_threshold",
      fareParametersId = B.fieldNamed "fare_parameters_id",
      distanceCalculationFailed = B.fieldNamed "distance_calculation_failed",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      driverDeviatedFromRoute = B.fieldNamed "driver_deviated_from_route",
      numberOfSnapToRoadCalls = B.fieldNamed "number_of_snap_to_road_calls",
      numberOfDeviation = B.fieldNamed "number_of_deviation",
      uiDistanceCalculationWithAccuracy = B.fieldNamed "ui_distance_calculation_with_accuracy",
      uiDistanceCalculationWithoutAccuracy = B.fieldNamed "ui_distance_calculation_without_accuracy"
    }

$(enableKVPG ''RideT ['id] [['bookingId], ['driverId]])

$(mkTableInstances ''RideT "ride" "atlas_driver_offer_bpp")
