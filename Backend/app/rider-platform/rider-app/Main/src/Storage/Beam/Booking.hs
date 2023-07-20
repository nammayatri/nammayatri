{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Booking where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Booking.Type as Domain
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant (..))
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.BookingStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BookingStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BookingStatus

instance FromBackendRow Postgres Domain.BookingStatus

instance IsString Domain.BookingStatus where
  fromString = show

data BookingT f = BookingT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    fareProductType :: B.C f DQuote.FareProductType,
    bppBookingId :: B.C f (Maybe Text),
    quoteId :: B.C f (Maybe Text),
    riderId :: B.C f Text,
    paymentMethodId :: B.C f (Maybe Text),
    paymentUrl :: B.C f (Maybe Text),
    status :: B.C f Domain.BookingStatus,
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    providerName :: B.C f Text,
    providerMobileNumber :: B.C f Text,
    primaryExophone :: B.C f Text,
    startTime :: B.C f Time.UTCTime,
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f (Maybe Text),
    estimatedFare :: B.C f HighPrecMoney,
    discount :: B.C f (Maybe HighPrecMoney),
    estimatedTotalFare :: B.C f HighPrecMoney,
    distance :: B.C f (Maybe HighPrecMeters),
    otpCode :: B.C f (Maybe Text),
    vehicleVariant :: B.C f VehVar.VehicleVariant,
    tripTermsId :: B.C f (Maybe Text),
    rentalSlabId :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta BookingT where
  modelFieldModification = bookingTMod
  modelTableName = "booking"
  modelSchemaName = Just "atlas_app"

type Booking = BookingT Identity

instance FromJSON Booking where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Booking where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Booking

bookingTMod :: BookingT (B.FieldModification (B.TableField BookingT))
bookingTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      fareProductType = B.fieldNamed "fare_product_type",
      bppBookingId = B.fieldNamed "bpp_ride_booking_id",
      quoteId = B.fieldNamed "quote_id",
      riderId = B.fieldNamed "rider_id",
      paymentMethodId = B.fieldNamed "payment_method_id",
      paymentUrl = B.fieldNamed "payment_url",
      status = B.fieldNamed "status",
      providerId = B.fieldNamed "provider_id",
      providerUrl = B.fieldNamed "provider_url",
      providerName = B.fieldNamed "provider_name",
      providerMobileNumber = B.fieldNamed "provider_mobile_number",
      primaryExophone = B.fieldNamed "primary_exophone",
      startTime = B.fieldNamed "start_time",
      fromLocationId = B.fieldNamed "from_location_id",
      toLocationId = B.fieldNamed "to_location_id",
      estimatedFare = B.fieldNamed "estimated_fare",
      discount = B.fieldNamed "discount",
      estimatedTotalFare = B.fieldNamed "estimated_total_fare",
      distance = B.fieldNamed "distance",
      otpCode = B.fieldNamed "otp_code",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      tripTermsId = B.fieldNamed "trip_terms_id",
      rentalSlabId = B.fieldNamed "rental_slab_id",
      merchantId = B.fieldNamed "merchant_id",
      specialLocationTag = B.fieldNamed "special_location_tag",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize Booking where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

bookingToHSModifiers :: M.Map Text (A.Value -> A.Value)
bookingToHSModifiers =
  M.empty

bookingToPSModifiers :: M.Map Text (A.Value -> A.Value)
bookingToPSModifiers =
  M.empty

$(enableKVPG ''BookingT ['id] [['bppBookingId], ['riderId]])
