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

module Storage.Beam.Driver.DriverFlowStatus where

import Data.Aeson
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
import qualified Domain.Types.Driver.DriverFlowStatus as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

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

instance FromField Domain.FlowStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.FlowStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.FlowStatus

instance FromBackendRow Postgres Domain.FlowStatus

instance IsString Domain.FlowStatus where
  fromString = show

data DriverFlowStatusT f = DriverFlowStatusT
  { personId :: B.C f Text,
    flowStatus :: B.C f Domain.FlowStatus,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverFlowStatusT where
  data PrimaryKey DriverFlowStatusT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . personId

instance ModelMeta DriverFlowStatusT where
  modelFieldModification = driverFlowStatusTMod
  modelTableName = "driver_flow_status"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type DriverFlowStatus = DriverFlowStatusT Identity

instance FromJSON DriverFlowStatus where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverFlowStatus where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverFlowStatus

deriving stock instance Ord Domain.FlowStatus

deriving stock instance Read Domain.FlowStatus

-- deriving stock instance Read (SearchRequest)

-- deriving stock instance Read (Merchant)

driverFlowStatusTMod :: DriverFlowStatusT (B.FieldModification (B.TableField DriverFlowStatusT))
driverFlowStatusTMod =
  B.tableModification
    { personId = B.fieldNamed "person_id",
      flowStatus = B.fieldNamed "flow_status",
      updatedAt = B.fieldNamed "updated_at"
    }

defaultDriverFlowStatus :: DriverFlowStatus
defaultDriverFlowStatus =
  DriverFlowStatusT
    { personId = "",
      flowStatus = "",
      updatedAt = defaultUTCDate
    }

instance Serialize DriverFlowStatus where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverFlowStatusToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverFlowStatusToHSModifiers =
  M.empty

driverFlowStatusToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverFlowStatusToPSModifiers =
  M.empty

$(enableKVPG ''DriverFlowStatusT ['personId] [])
