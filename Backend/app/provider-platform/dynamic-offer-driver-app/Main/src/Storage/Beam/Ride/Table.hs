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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Ride.Table where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Ride as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (HighPrecMeters, Meters, Money)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Booking (BookingTId)
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Person (PersonTId)

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance IsString Meters where
  fromString = show

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

deriving stock instance Read Money

instance IsString Money where
  fromString = show

instance FromField Domain.RideStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.RideStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.RideStatus

instance FromBackendRow Postgres Domain.RideStatus

instance IsString Domain.RideStatus where
  fromString = show

instance FromField HighPrecMeters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be HighPrecMeters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMeters

instance FromBackendRow Postgres HighPrecMeters

instance IsString HighPrecMeters where
  fromString = show

data RideT f = RideT
  { id :: B.C f Text,
    bookingId :: B.C f Text,
    shortId :: B.C f Text,
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
    fareParametersId :: B.C f (Maybe Text),
    distanceCalculationFailed :: B.C f (Maybe Bool),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RideT where
  data PrimaryKey RideT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta RideT where
  modelFieldModification = rideTMod
  modelTableName = "ride"
  mkExprWithDefault _ = B.insertExpressions []

type Ride = RideT Identity

instance FromJSON Ride where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Ride where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Ride

rideTMod :: RideT (B.FieldModification (B.TableField RideT))
rideTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      bookingId = B.fieldNamed "booking_id",
      shortId = B.fieldNamed "short_id",
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
      fareParametersId = B.fieldNamed "fare_parameters_id",
      distanceCalculationFailed = B.fieldNamed "distance_calculation_failed",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }


instance IsString Domain.RideStatus where
  fromString = show

instance IsString Money where
  fromString = show

instance IsString HighPrecMeters where
  fromString = show

instance IsString Meters where
  fromString = show

defaultRide :: Ride
defaultRide =
  RideT
    { id = "",
      bookingId = "",
      shortId = "",
      status = "",
      driverId = "",
      otp = "",
      trackingUrl = "",
      fare = Nothing,
      traveledDistance = "",
      chargeableDistance = Nothing,
      driverArrivalTime = Nothing,
      tripStartTime = Nothing,
      tripEndTime = Nothing,
      tripStartLat = Nothing,
      tripStartLon = Nothing,
      tripEndLat = Nothing,
      tripEndLon = Nothing,
      fareParametersId = Nothing,
      distanceCalculationFailed = Nothing,
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize Table where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

tableToHSModifiers :: M.Map Text (A.Value -> A.Value)
tableToHSModifiers =
  M.fromList
    []

tableToPSModifiers :: M.Map Text (A.Value -> A.Value)
tableToPSModifiers =
  M.fromList
    []

$(enableKVPG ''RideT ['id] [])
