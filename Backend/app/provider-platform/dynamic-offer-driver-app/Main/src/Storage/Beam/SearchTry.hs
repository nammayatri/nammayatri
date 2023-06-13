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

module Storage.Beam.SearchTry where

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
import qualified Domain.Types.SearchTry as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()

instance FromField Domain.SearchTryStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.SearchTryStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.SearchTryStatus

instance FromBackendRow Postgres Domain.SearchTryStatus

instance FromField Domain.SearchRepeatType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.SearchRepeatType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.SearchRepeatType

instance FromBackendRow Postgres Domain.SearchRepeatType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be BaseUrl where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be BaseUrl

-- instance FromBackendRow Postgres Money

data SearchTryT f = SearchTryT
  { id :: B.C f Text,
    messageId :: B.C f Text,
    requestId :: B.C f Text,
    startTime :: B.C f Time.UTCTime,
    validTill :: B.C f Time.UTCTime,
    estimateId :: B.C f Text,
    baseFare :: B.C f Money,
    customerExtraFee :: B.C f (Maybe Money),
    status :: B.C f Domain.SearchTryStatus,
    vehicleVariant :: B.C f Variant.Variant,
    searchRepeatCounter :: B.C f Int,
    searchRepeatType :: B.C f Domain.SearchRepeatType,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

-- instance IsString Domain.SearchTryStatus where
--   fromString = show

instance IsString Variant.Variant where
  fromString = show

instance IsString Meters where
  fromString = show

instance IsString Seconds where
  fromString = show

instance B.Table SearchTryT where
  data PrimaryKey SearchTryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SearchTryT where
  modelFieldModification = searchTryTMod
  modelTableName = "search_try"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type SearchTry = SearchTryT Identity

instance FromJSON SearchTry where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SearchTry where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SearchTry

-- deriving stock instance Read Money

searchTryTMod :: SearchTryT (B.FieldModification (B.TableField SearchTryT))
searchTryTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      messageId = B.fieldNamed "message_id",
      requestId = B.fieldNamed "request_id",
      estimateId = B.fieldNamed "estimate_id",
      startTime = B.fieldNamed "start_time",
      validTill = B.fieldNamed "valid_till",
      baseFare = B.fieldNamed "base_fare",
      customerExtraFee = B.fieldNamed "customer_extra_fee",
      status = B.fieldNamed "status",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      searchRepeatCounter = B.fieldNamed "search_repeat_counter",
      searchRepeatType = B.fieldNamed "search_repeat_type",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

searchTryToHSModifiers :: M.Map Text (A.Value -> A.Value)
searchTryToHSModifiers =
  M.empty

searchTryToPSModifiers :: M.Map Text (A.Value -> A.Value)
searchTryToPSModifiers =
  M.empty

-- defaultSearchTry :: SearchTry
-- defaultSearchTry =
--   SearchTryT
--     { id = "",
--       transactionId = "",
--       messageId = "",
--       estimateId = "",
--       startTime = defaultUTCDate,
--       validTill = defaultUTCDate,
--       providerId = "",
--       fromLocationId = "",
--       toLocationId = "",
--       bapId = "",
--       bapUri = "",
--       estimatedDistance = "",
--       estimatedDuration = "",
--       customerExtraFee = Nothing,
--       device = Nothing,
--       status = "",
--       vehicleVariant = "",
--       searchRepeatCounter = 0,
--       autoAssignEnabled = False,
--       createdAt = defaultUTCDate,
--       updatedAt = defaultUTCDate
--     }

instance Serialize SearchTry where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''SearchTryT ['id] [])
