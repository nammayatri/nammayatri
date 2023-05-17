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

module Storage.Beam.Booking where

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
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Vehicle.Variant as Veh
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Booking.BookingLocation hiding (createdAt, id, updatedAt)
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
import Storage.Tabular.Vehicle ()

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

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

instance FromField Seconds where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance FromField Domain.BookingStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingStatus

instance FromBackendRow Postgres Domain.BookingStatus

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Domain.BookingType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingType

instance FromBackendRow Postgres Domain.BookingType

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

deriving stock instance Read Money

instance FromField Veh.Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Veh.Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Veh.Variant

instance FromBackendRow Postgres Veh.Variant

data BookingT f = BookingT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    quoteId :: B.C f Text,
    status :: B.C f Domain.BookingStatus,
    bookingType :: B.C f Domain.BookingType,
    specialZoneOtpCode :: B.C f (Maybe Text),
    providerId :: B.C f Text,
    primaryExophone :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    startTime :: B.C f Time.LocalTime,
    riderId :: B.C f (Maybe Text),
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    vehicleVariant :: B.C f Veh.Variant,
    estimatedDistance :: B.C f Meters,
    maxEstimatedDistance :: B.C f (Maybe Centesimal),
    estimatedFare :: B.C f Money,
    estimatedDuration :: B.C f Seconds,
    fareParametersId :: B.C f Text,
    riderName :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance IsString Domain.BookingStatus where
  fromString = show

instance IsString Domain.BookingType where
  fromString = show

instance IsString Veh.Variant where
  fromString = show

instance IsString Money where
  fromString = show

instance IsString Meters where
  fromString = show

instance IsString Seconds where
  fromString = show

instance B.Table BookingT where
  data PrimaryKey BookingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta BookingT where
  modelFieldModification = bookingTMod
  modelTableName = "booking"
  mkExprWithDefault _ = B.insertExpressions []

type Booking = BookingT Identity

instance FromJSON Booking where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Booking where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Booking

deriving stock instance Read Money

bookingTMod :: BookingT (B.FieldModification (B.TableField BookingT))
bookingTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      quoteId = B.fieldNamed "quote_id",
      status = B.fieldNamed "status",
      bookingType = B.fieldNamed "booking_type",
      specialZoneOtpCode = B.fieldNamed "special_zone_otp_code",
      providerId = B.fieldNamed "provider_id",
      primaryExophone = B.fieldNamed "primary_exophone",
      bapId = B.fieldNamed "bap_id",
      bapUri = B.fieldNamed "bap_uri",
      startTime = B.fieldNamed "start_time",
      riderId = B.fieldNamed "rider_id",
      fromLocationId = B.fieldNamed "from_location_id",
      toLocationId = B.fieldNamed "to_location_id",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      estimatedDistance = B.fieldNamed "estimated_distance",
      maxEstimatedDistance = B.fieldNamed "max_estimated_distance",
      estimatedFare = B.fieldNamed "estimated_fare",
      estimatedDuration = B.fieldNamed "estimated_duration",
      fareParametersId = B.fieldNamed "fare_parameters_id",
      riderName = B.fieldNamed "rider_name",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

bookingToHSModifiers :: M.Map Text (A.Value -> A.Value)
bookingToHSModifiers =
  M.fromList
    []

bookingToPSModifiers :: M.Map Text (A.Value -> A.Value)
bookingToPSModifiers =
  M.fromList
    []

defaultBooking :: Booking
defaultBooking =
  BookingT
    { id = "",
      transactionId = "",
      quoteId = "",
      status = "",
      bookingType = "",
      specialZoneOtpCode = Nothing,
      providerId = "",
      primaryExophone = "",
      bapId = "",
      bapUri = "",
      startTime = defaultDate,
      riderId = Nothing,
      fromLocationId = "",
      toLocationId = "",
      vehicleVariant = "",
      estimatedDistance = "",
      maxEstimatedDistance = Nothing,
      estimatedFare = "",
      estimatedDuration = "",
      fareParametersId = "",
      riderName = Nothing,
      createdAt = defaultDate,
      updatedAt = defaultDate
    }

instance Serialize Booking where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''BookingT ['id] [])
