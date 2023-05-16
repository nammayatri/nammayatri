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

module Storage.Beam.Merchant.DriverPoolConfig where

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
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.DriverPoolConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Meters, Seconds)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config (PoolSortingType (..))
import Storage.Tabular.Merchant (MerchantTId)

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

instance FromField Seconds where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance FromField PoolSortingType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PoolSortingType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be PoolSortingType

instance FromBackendRow Postgres PoolSortingType

data DriverPoolConfigT f = DriverPoolConfigT
  { merchantId :: B.C f Text,
    minRadiusOfSearch :: B.C f Meters,
    maxRadiusOfSearch :: B.C f Meters,
    radiusStepSize :: B.C f Meters,
    driverPositionInfoExpiry :: B.C f (Maybe Seconds),
    actualDistanceThreshold :: B.C f (Maybe Meters),
    maxDriverQuotesRequired :: B.C f Int,
    maxParallelSearchRequests :: B.C f Int,
    driverQuoteLimit :: B.C f Int,
    driverRequestCountLimit :: B.C f Int,
    driverBatchSize :: B.C f Int,
    maxNumberOfBatches :: B.C f Int,
    poolSortingType :: B.C f PoolSortingType,
    singleBatchProcessTime :: B.C f Seconds,
    tripDistance :: B.C f Meters,
    radiusShrinkValueForDriversOnRide :: B.C f Int,
    driverToDestinationDistanceThreshold :: B.C f Meters,
    driverToDestinationDuration :: B.C f Seconds,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverPoolConfigT where
  data PrimaryKey DriverPoolConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

instance ModelMeta DriverPoolConfigT where
  modelFieldModification = driverPoolConfigTMod
  modelTableName = "driver_pool_config"
  mkExprWithDefault _ = B.insertExpressions []

type DriverPoolConfig = DriverPoolConfigT Identity

instance FromJSON DriverPoolConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverPoolConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverPoolConfig

deriving stock instance Ord PoolSortingType

deriving stock instance Eq PoolSortingType

driverPoolConfigTMod :: DriverPoolConfigT (B.FieldModification (B.TableField DriverPoolConfigT))
driverPoolConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      minRadiusOfSearch = B.fieldNamed "min_radius_of_search",
      maxRadiusOfSearch = B.fieldNamed "max_radius_of_search",
      radiusStepSize = B.fieldNamed "radius_step_size",
      driverPositionInfoExpiry = B.fieldNamed "driver_position_info_expiry",
      actualDistanceThreshold = B.fieldNamed "actual_distance_threshold",
      maxDriverQuotesRequired = B.fieldNamed "max_driver_quotes_required",
      maxParallelSearchRequests = B.fieldNamed "max_parallel_search_requests",
      driverQuoteLimit = B.fieldNamed "driver_quote_limit",
      driverRequestCountLimit = B.fieldNamed "driver_request_count_limit",
      driverBatchSize = B.fieldNamed "driver_batch_size",
      maxNumberOfBatches = B.fieldNamed "max_number_of_batches",
      poolSortingType = B.fieldNamed "pool_sorting_type",
      singleBatchProcessTime = B.fieldNamed "single_batch_process_time",
      tripDistance = B.fieldNamed "trip_distance",
      radiusShrinkValueForDriversOnRide = B.fieldNamed "radius_shrink_value_for_drivers_on_ride",
      driverToDestinationDistanceThreshold = B.fieldNamed "driver_to_destination_distance_threshold",
      driverToDestinationDuration = B.fieldNamed "driver_to_destination_duration",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverPoolConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverPoolConfigToHSModifiers =
  M.fromList
    []

driverPoolConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverPoolConfigToPSModifiers =
  M.fromList
    []

instance IsString Meters where
  fromString = show

instance IsString PoolSortingType where
  fromString = show

instance IsString Seconds where
  fromString = show

defaultDriverPoolConfig :: DriverPoolConfig
defaultDriverPoolConfig =
  DriverPoolConfigT
    { merchantId = "",
      minRadiusOfSearch = "",
      maxRadiusOfSearch = "",
      radiusStepSize = "",
      driverPositionInfoExpiry = Nothing,
      actualDistanceThreshold = Nothing,
      maxDriverQuotesRequired = 0,
      maxParallelSearchRequests = 0,
      driverQuoteLimit = 0,
      driverRequestCountLimit = 0,
      driverBatchSize = 0,
      maxNumberOfBatches = 0,
      poolSortingType = "",
      singleBatchProcessTime = "",
      tripDistance = "",
      radiusShrinkValueForDriversOnRide = 0,
      driverToDestinationDistanceThreshold = "",
      driverToDestinationDuration = "",
      createdAt = defaultDate,
      updatedAt = defaultDate
    }

instance Serialize DriverPoolConfig where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverPoolConfigT ['tripDistance] [])
