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

module Storage.Beam.SearchRequestForDriver where

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
import qualified Domain.Types.DriverInformation as D
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()

instance FromField D.DriverMode where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be D.DriverMode where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be D.DriverMode

instance FromBackendRow Postgres D.DriverMode

instance FromField Domain.DriverSearchRequestStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.DriverSearchRequestStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.DriverSearchRequestStatus

instance FromBackendRow Postgres Domain.DriverSearchRequestStatus

instance FromField Domain.SearchRequestForDriverResponse where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.SearchRequestForDriverResponse where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.SearchRequestForDriverResponse

instance FromBackendRow Postgres Domain.SearchRequestForDriverResponse

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
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

-- instance IsString Money where
--   fromString = show

instance IsString Domain.DriverSearchRequestStatus where
  fromString = show

instance IsString Domain.SearchRequestForDriverResponse where
  fromString = show

instance IsString Variant.Variant where
  fromString = show

instance B.Table SearchRequestForDriverT where
  data PrimaryKey SearchRequestForDriverT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SearchRequestForDriverT where
  modelFieldModification = searchRequestForDriverTMod
  modelTableName = "search_request_for_driver"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type SearchRequestForDriver = SearchRequestForDriverT Identity

instance FromJSON SearchRequestForDriver where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SearchRequestForDriver where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON Domain.DriverSearchRequestStatus where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.DriverSearchRequestStatus where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SearchRequestForDriver

deriving stock instance Ord Domain.DriverSearchRequestStatus

deriving stock instance Ord Domain.SearchRequestForDriverResponse

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
      createdAt = B.fieldNamed "created_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

searchRequestForDriverToHSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestForDriverToHSModifiers =
  M.empty

searchRequestForDriverToPSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestForDriverToPSModifiers =
  M.empty

instance Serialize SearchRequestForDriver where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''SearchRequestForDriverT ['id] [['driverId], ['searchTryId], ['requestId]])
