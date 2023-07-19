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

module Storage.Beam.Quote where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
-- import Database.Beam.Backend
import Database.Beam.MySQL ()
-- import Database.Beam.Postgres
--   ( Postgres,
--   )
-- import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.FarePolicy.FareProductType as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data QuoteT f = QuoteT
  { id :: B.C f Text,
    fareProductType :: B.C f Domain.FareProductType,
    requestId :: B.C f Text,
    estimatedFare :: B.C f HighPrecMoney,
    discount :: B.C f (Maybe HighPrecMoney),
    estimatedTotalFare :: B.C f HighPrecMoney,
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    providerName :: B.C f Text,
    providerMobileNumber :: B.C f Text,
    providerCompletedRidesCount :: B.C f Int,
    distanceToNearestDriver :: B.C f (Maybe HighPrecMeters),
    vehicleVariant :: B.C f VehVar.VehicleVariant,
    tripTermsId :: B.C f (Maybe Text),
    rentalSlabId :: B.C f (Maybe Text),
    driverOfferId :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    specialZoneQuoteId :: B.C f (Maybe Text),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteT where
  data PrimaryKey QuoteT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta QuoteT where
  modelFieldModification = quoteTMod
  modelTableName = "quote"
  modelSchemaName = Just "atlas_app"

type Quote = QuoteT Identity

instance FromJSON Quote where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Quote where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Quote

quoteTMod :: QuoteT (B.FieldModification (B.TableField QuoteT))
quoteTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      fareProductType = B.fieldNamed "fare_product_type",
      requestId = B.fieldNamed "request_id",
      estimatedFare = B.fieldNamed "estimated_fare",
      discount = B.fieldNamed "discount",
      estimatedTotalFare = B.fieldNamed "estimated_total_fare",
      providerId = B.fieldNamed "provider_id",
      providerUrl = B.fieldNamed "provider_url",
      providerName = B.fieldNamed "provider_name",
      providerMobileNumber = B.fieldNamed "provider_mobile_number",
      providerCompletedRidesCount = B.fieldNamed "provider_completed_rides_count",
      distanceToNearestDriver = B.fieldNamed "distance_to_nearest_driver",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      tripTermsId = B.fieldNamed "trip_terms_id",
      rentalSlabId = B.fieldNamed "rental_slab_id",
      driverOfferId = B.fieldNamed "driver_offer_id",
      merchantId = B.fieldNamed "merchant_id",
      specialZoneQuoteId = B.fieldNamed "special_zone_quote_id",
      specialLocationTag = B.fieldNamed "special_location_tag",
      createdAt = B.fieldNamed "created_at"
    }

instance Serialize Quote where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

quoteToHSModifiers :: M.Map Text (A.Value -> A.Value)
quoteToHSModifiers =
  M.empty

quoteToPSModifiers :: M.Map Text (A.Value -> A.Value)
quoteToPSModifiers =
  M.empty

$(enableKVPG ''QuoteT ['id] [['providerId], ['requestId]])
