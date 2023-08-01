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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Estimate where

import Data.Serialize
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Estimate as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Maps hiding (status)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.EstimateStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.EstimateStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.EstimateStatus

instance FromBackendRow Postgres Domain.EstimateStatus

instance IsString Domain.EstimateStatus where
  fromString = show

instance IsString TimeOfDay where
  fromString = show

instance FromField [LatLong] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance FromField LatLong where
  fromField = fromFieldEnum

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [LatLong] where
  sqlValueSyntax latlonglist =
    let x = (show <$> latlonglist :: [Text])
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [LatLong]

instance FromBackendRow Postgres [LatLong]

deriving stock instance Ord LatLong

data EstimateT f = EstimateT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    bppEstimateId :: B.C f Text,
    itemId :: B.C f Text,
    estimatedFare :: B.C f HighPrecMoney,
    discount :: B.C f (Maybe HighPrecMoney),
    estimatedTotalFare :: B.C f HighPrecMoney,
    minTotalFare :: B.C f HighPrecMoney,
    maxTotalFare :: B.C f HighPrecMoney,
    estimatedDuration :: B.C f (Maybe Seconds),
    estimatedDistance :: B.C f (Maybe HighPrecMeters),
    device :: B.C f (Maybe Text),
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    providerName :: B.C f Text,
    providerMobileNumber :: B.C f Text,
    providerCompletedRidesCount :: B.C f Int,
    vehicleVariant :: B.C f VehVar.VehicleVariant,
    driversLocation :: B.C f [LatLong],
    tripTermsId :: B.C f (Maybe Text),
    nightShiftCharge :: B.C f (Maybe Money),
    oldNightShiftCharge :: B.C f (Maybe Centesimal),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    status :: B.C f Domain.EstimateStatus,
    waitingChargePerMin :: B.C f (Maybe Money),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateT where
  data PrimaryKey EstimateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Estimate = EstimateT Identity

estimateTMod :: EstimateT (B.FieldModification (B.TableField EstimateT))
estimateTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      requestId = B.fieldNamed "request_id",
      merchantId = B.fieldNamed "merchant_id",
      bppEstimateId = B.fieldNamed "bpp_estimate_id",
      estimatedFare = B.fieldNamed "estimated_fare",
      itemId = B.fieldNamed "item_id",
      discount = B.fieldNamed "discount",
      estimatedTotalFare = B.fieldNamed "estimated_total_fare",
      minTotalFare = B.fieldNamed "min_total_fare",
      maxTotalFare = B.fieldNamed "max_total_fare",
      estimatedDuration = B.fieldNamed "estimated_duration",
      estimatedDistance = B.fieldNamed "estimated_distance",
      device = B.fieldNamed "device",
      providerId = B.fieldNamed "provider_id",
      providerUrl = B.fieldNamed "provider_url",
      providerName = B.fieldNamed "provider_name",
      providerMobileNumber = B.fieldNamed "provider_mobile_number",
      providerCompletedRidesCount = B.fieldNamed "provider_completed_rides_count",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      driversLocation = B.fieldNamed "drivers_location",
      tripTermsId = B.fieldNamed "trip_terms_id",
      nightShiftCharge = B.fieldNamed "night_shift_charge",
      oldNightShiftCharge = B.fieldNamed "night_shift_multiplier",
      nightShiftStart = B.fieldNamed "night_shift_start",
      nightShiftEnd = B.fieldNamed "night_shift_end",
      status = B.fieldNamed "status",
      waitingChargePerMin = B.fieldNamed "waiting_charge_per_min",
      specialLocationTag = B.fieldNamed "special_location_tag",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''EstimateT ['id] [['requestId], ['bppEstimateId]])

$(mkTableInstances ''EstimateT "estimate" "atlas_app")
