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

module Storage.Beam.Booking where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Vehicle.Variant as Veh
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.BookingStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingStatus

instance FromBackendRow Postgres Domain.BookingStatus

instance FromField Domain.BookingType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingType

instance FromBackendRow Postgres Domain.BookingType

data BookingT f = BookingT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    quoteId :: B.C f Text,
    status :: B.C f Domain.BookingStatus,
    bookingType :: B.C f Domain.BookingType,
    specialLocationTag :: B.C f (Maybe Text),
    specialZoneOtpCode :: B.C f (Maybe Text),
    disabilityTag :: B.C f (Maybe Text),
    area :: B.C f (Maybe FareProductD.Area),
    providerId :: B.C f Text,
    primaryExophone :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    bapCity :: B.C f (Maybe Context.City),
    bapCountry :: B.C f (Maybe Context.Country),
    startTime :: B.C f Time.UTCTime,
    riderId :: B.C f (Maybe Text),
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    vehicleVariant :: B.C f Veh.Variant,
    estimatedDistance :: B.C f Meters,
    maxEstimatedDistance :: B.C f (Maybe HighPrecMeters),
    estimatedFare :: B.C f Money,
    estimatedDuration :: B.C f Seconds,
    fareParametersId :: B.C f Text,
    riderName :: B.C f (Maybe Text),
    paymentUrl :: B.C f (Maybe Text),
    paymentMethodId :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance IsString Domain.BookingStatus where
  fromString = show

instance IsString Domain.BookingType where
  fromString = show

instance IsString Veh.Variant where
  fromString = show

instance B.Table BookingT where
  data PrimaryKey BookingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Booking = BookingT Identity

deriving stock instance Ord Context.City

deriving stock instance Ord Context.Country

bookingTMod :: BookingT (B.FieldModification (B.TableField BookingT))
bookingTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      quoteId = B.fieldNamed "quote_id",
      status = B.fieldNamed "status",
      specialLocationTag = B.fieldNamed "special_location_tag",
      disabilityTag = B.fieldNamed "disability_tag",
      bookingType = B.fieldNamed "booking_type",
      specialZoneOtpCode = B.fieldNamed "special_zone_otp_code",
      area = B.fieldNamed "area",
      providerId = B.fieldNamed "provider_id",
      primaryExophone = B.fieldNamed "primary_exophone",
      bapId = B.fieldNamed "bap_id",
      bapUri = B.fieldNamed "bap_uri",
      bapCity = B.fieldNamed "bap_city",
      bapCountry = B.fieldNamed "bap_country",
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
      paymentUrl = B.fieldNamed "payment_url",
      createdAt = B.fieldNamed "created_at",
      paymentMethodId = B.fieldNamed "payment_method_id",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''BookingT ['id] [['specialZoneOtpCode], ['quoteId]])

$(mkTableInstances ''BookingT "booking" "atlas_driver_offer_bpp")
