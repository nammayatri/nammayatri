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

module Storage.Beam.Person.PersonFlowStatus where

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
import qualified Domain.Types.Person.PersonFlowStatus as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Text (encodeToText)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.FlowStatus where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Domain.FlowStatus where
  sqlValueSyntax = sqlValueSyntax . encodeToText

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.FlowStatus

instance FromBackendRow Postgres Domain.FlowStatus

instance IsString Domain.FlowStatus where
  fromString = show

deriving stock instance Ord Domain.FlowStatus

data PersonFlowStatusT f = PersonFlowStatusT
  { personId :: B.C f Text,
    flowStatus :: B.C f Domain.FlowStatus,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonFlowStatusT where
  data PrimaryKey PersonFlowStatusT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . personId

instance ModelMeta PersonFlowStatusT where
  modelFieldModification = personFlowStatusTMod
  modelTableName = "person_flow_status"
  modelSchemaName = Just "atlas_app"

type PersonFlowStatus = PersonFlowStatusT Identity

instance FromJSON PersonFlowStatus where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON PersonFlowStatus where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show PersonFlowStatus

personFlowStatusTMod :: PersonFlowStatusT (B.FieldModification (B.TableField PersonFlowStatusT))
personFlowStatusTMod =
  B.tableModification
    { personId = B.fieldNamed "person_id",
      flowStatus = B.fieldNamed "flow_status",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize PersonFlowStatus where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

personFlowStatusToHSModifiers :: M.Map Text (A.Value -> A.Value)
personFlowStatusToHSModifiers =
  M.empty

personFlowStatusToPSModifiers :: M.Map Text (A.Value -> A.Value)
personFlowStatusToPSModifiers =
  M.empty

$(enableKVPG ''PersonFlowStatusT ['personId] [])
