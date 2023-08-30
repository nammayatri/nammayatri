{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverQuote where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.DriverQuote as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Common as Common
import Lib.Utils ()
import Sequelize

data DriverQuoteT f = DriverQuoteT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    searchTryId :: B.C f Text,
    searchRequestForDriverId :: B.C f (Maybe Text),
    estimateId :: B.C f Text,
    driverId :: B.C f Text,
    driverName :: B.C f Text,
    driverRating :: B.C f (Maybe Centesimal),
    status :: B.C f Domain.DriverQuoteStatus,
    vehicleVariant :: B.C f Variant.Variant,
    distance :: B.C f Meters,
    distanceToPickup :: B.C f Meters,
    durationToPickup :: B.C f Seconds,
    validTill :: B.C f Time.LocalTime,
    goHomeRequestId :: B.C f (Maybe Text),
    estimatedFare :: B.C f Common.Money,
    fareParametersId :: B.C f Text,
    providerId :: B.C f Text,
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverQuoteT where
  data PrimaryKey DriverQuoteT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverQuote = DriverQuoteT Identity

driverQuoteTMod :: DriverQuoteT (B.FieldModification (B.TableField DriverQuoteT))
driverQuoteTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      requestId = B.fieldNamed "search_request_id",
      searchTryId = B.fieldNamed "search_try_id",
      searchRequestForDriverId = B.fieldNamed "search_request_for_driver_id",
      driverId = B.fieldNamed "driver_id",
      estimateId = B.fieldNamed "estimate_id",
      driverName = B.fieldNamed "driver_name",
      driverRating = B.fieldNamed "driver_rating",
      status = B.fieldNamed "status",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      distance = B.fieldNamed "distance",
      distanceToPickup = B.fieldNamed "distance_to_pickup",
      durationToPickup = B.fieldNamed "duration_to_pickup",
      validTill = B.fieldNamed "valid_till",
      goHomeRequestId = B.fieldNamed "go_home_request_id",
      estimatedFare = B.fieldNamed "estimated_fare",
      fareParametersId = B.fieldNamed "fare_parameters_id",
      providerId = B.fieldNamed "provider_id",
      specialLocationTag = B.fieldNamed "special_location_tag",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''DriverQuoteT ['id] [['driverId], ['searchTryId], ['requestId]])

$(mkTableInstances ''DriverQuoteT "driver_quote" "atlas_driver_offer_bpp")
