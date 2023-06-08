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

module Storage.Beam.SearchRequest where

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
-- import qualified Domain.Types.SearchRequest as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()
import qualified Tools.Maps as Maps

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

-- instance FromField Variant.Variant where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant.Variant where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Variant.Variant

-- instance FromBackendRow Postgres Variant.Variant

-- instance FromField Seconds where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

-- instance FromBackendRow Postgres Seconds

-- instance FromField Domain.SearchRequestStatus where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.SearchRequestStatus where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.SearchRequestStatus

-- instance FromBackendRow Postgres Domain.SearchRequestStatus

-- instance FromField Meters where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

-- instance FromBackendRow Postgres Meters

-- instance FromField Money where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance HasSqlValueSyntax be String => HasSqlValueSyntax be BaseUrl where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be BaseUrl

-- instance FromBackendRow Postgres Money

data SearchRequestT f = SearchRequestT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    providerId :: B.C f Text,
    fromLocationId :: B.C f Text,
    toLocationId :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    estimatedDistance :: B.C f Meters,
    estimatedDuration :: B.C f Seconds,
    customerLanguage :: B.C f (Maybe Maps.Language),
    device :: B.C f (Maybe Text),
    autoAssignEnabled :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

-- instance IsString Domain.SearchRequestStatus where
--   fromString = show

instance IsString Variant.Variant where
  fromString = show

instance IsString Meters where
  fromString = show

instance IsString Seconds where
  fromString = show

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SearchRequestT where
  modelFieldModification = searchRequestTMod
  modelTableName = "search_request"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type SearchRequest = SearchRequestT Identity

instance FromJSON SearchRequest where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SearchRequest where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SearchRequest

instance FromField Maps.Language where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Maps.Language where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Maps.Language

instance FromBackendRow Postgres Maps.Language

-- deriving stock instance Read Money

searchRequestTMod :: SearchRequestT (B.FieldModification (B.TableField SearchRequestT))
searchRequestTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      providerId = B.fieldNamed "provider_id",
      fromLocationId = B.fieldNamed "from_location_id",
      toLocationId = B.fieldNamed "to_location_id",
      bapId = B.fieldNamed "bap_id",
      bapUri = B.fieldNamed "bap_uri",
      estimatedDistance = B.fieldNamed "estimated_distance",
      estimatedDuration = B.fieldNamed "estimated_duration",
      customerLanguage = B.fieldNamed "customer_language",
      device = B.fieldNamed "device",
      autoAssignEnabled = B.fieldNamed "auto_assign_enabled",
      createdAt = B.fieldNamed "created_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

searchRequestToHSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestToHSModifiers =
  M.empty

searchRequestToPSModifiers :: M.Map Text (A.Value -> A.Value)
searchRequestToPSModifiers =
  M.empty

-- defaultSearchRequest :: SearchRequest
-- defaultSearchRequest =
--   SearchRequestT
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

instance Serialize SearchRequest where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''SearchRequestT ['id] [])
