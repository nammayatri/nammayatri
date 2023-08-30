{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.SearchRequestForDriver where

import Data.ByteString
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.DriverInformation as D
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.Types
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

extractValue :: KVDBAnswer [ByteString] -> [ByteString]
extractValue (Right value) = value
extractValue _ = []

searchReqestForDriverkey :: Text -> Text
searchReqestForDriverkey prefix = "searchRequestForDriver_" <> prefix

data SearchRequestForDriverT f = SearchRequestForDriverT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    searchTryId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    startTime :: B.C f Time.UTCTime,
    actualDistanceToPickup :: B.C f Meters,
    straightLineDistanceToPickup :: B.C f Meters,
    durationToPickup :: B.C f Seconds,
    vehicleVariant :: B.C f Variant.Variant,
    batchNumber :: B.C f Int,
    lat :: B.C f (Maybe Double),
    lon :: B.C f (Maybe Double),
    searchRequestValidTill :: B.C f Time.LocalTime,
    driverId :: B.C f Text,
    status :: B.C f Domain.DriverSearchRequestStatus,
    response :: B.C f (Maybe Domain.SearchRequestForDriverResponse),
    driverMinExtraFee :: B.C f (Maybe Money),
    driverMaxExtraFee :: B.C f (Maybe Money),
    rideRequestPopupDelayDuration :: B.C f Seconds,
    isPartOfIntelligentPool :: B.C f Bool,
    cancellationRatio :: B.C f (Maybe Double),
    acceptanceRatio :: B.C f (Maybe Double),
    driverAvailableTime :: B.C f (Maybe Double),
    parallelSearchRequestCount :: B.C f (Maybe Int),
    driverSpeed :: B.C f (Maybe Double),
    keepHiddenForSeconds :: B.C f Seconds,
    mode :: B.C f (Maybe D.DriverMode),
    goHomeRequestId :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestForDriverT where
  data PrimaryKey SearchRequestForDriverT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchRequestForDriver = SearchRequestForDriverT Identity

searchRequestForDriverTMod :: SearchRequestForDriverT (B.FieldModification (B.TableField SearchRequestForDriverT))
searchRequestForDriverTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      requestId = B.fieldNamed "search_request_id",
      searchTryId = B.fieldNamed "search_try_id",
      merchantId = B.fieldNamed "merchant_id",
      startTime = B.fieldNamed "start_time",
      actualDistanceToPickup = B.fieldNamed "actual_distance_to_pickup",
      straightLineDistanceToPickup = B.fieldNamed "straight_line_distance_to_pickup",
      durationToPickup = B.fieldNamed "duration_to_pickup",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      batchNumber = B.fieldNamed "batch_number",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      searchRequestValidTill = B.fieldNamed "search_request_valid_till",
      driverId = B.fieldNamed "driver_id",
      status = B.fieldNamed "status",
      response = B.fieldNamed "response",
      driverMinExtraFee = B.fieldNamed "driver_min_extra_fee",
      driverMaxExtraFee = B.fieldNamed "driver_max_extra_fee",
      rideRequestPopupDelayDuration = B.fieldNamed "ride_request_popup_delay_duration",
      isPartOfIntelligentPool = B.fieldNamed "is_part_of_intelligent_pool",
      cancellationRatio = B.fieldNamed "cancellation_ratio",
      acceptanceRatio = B.fieldNamed "acceptance_ratio",
      driverAvailableTime = B.fieldNamed "driver_available_time",
      parallelSearchRequestCount = B.fieldNamed "parallel_search_request_count",
      driverSpeed = B.fieldNamed "driver_speed",
      keepHiddenForSeconds = B.fieldNamed "keep_hidden_for_seconds",
      mode = B.fieldNamed "mode",
      goHomeRequestId = B.fieldNamed "go_home_request_id",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''SearchRequestForDriverT ['id] [['searchTryId], ['requestId]])

$(mkTableInstances ''SearchRequestForDriverT "search_request_for_driver" "atlas_driver_offer_bpp")
