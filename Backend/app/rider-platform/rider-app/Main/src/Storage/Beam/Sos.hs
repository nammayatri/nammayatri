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

module Storage.Beam.Sos where

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
import qualified Domain.Types.Sos as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

instance FromField Domain.SosType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.SosType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.SosType

instance FromBackendRow Postgres Domain.SosType

instance IsString Domain.SosType where
  fromString = show

instance FromField Domain.SosStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.SosStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.SosStatus

instance FromBackendRow Postgres Domain.SosStatus

instance IsString Domain.SosStatus where
  fromString = show

data SosT f = SosT
  { id :: B.C f Text,
    personId :: B.C f Text,
    rideId :: B.C f Text,
    flow :: B.C f Domain.SosType,
    status :: B.C f Domain.SosStatus,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SosT where
  data PrimaryKey SosT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SosT where
  modelFieldModification = sosTMod
  modelTableName = "sos"
  modelSchemaName = Just "atlas_app"

type Sos = SosT Identity

instance FromJSON Sos where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Sos where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Sos

sosTMod :: SosT (B.FieldModification (B.TableField SosT))
sosTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      personId = B.fieldNamed "person_id",
      rideId = B.fieldNamed "ride_id",
      flow = B.fieldNamed "flow",
      status = B.fieldNamed "status",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

defaultSos :: Sos
defaultSos =
  SosT
    { id = "",
      personId = "",
      rideId = "",
      flow = "",
      status = "",
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize Sos where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

sosToHSModifiers :: M.Map Text (A.Value -> A.Value)
sosToHSModifiers =
  M.empty

sosToPSModifiers :: M.Map Text (A.Value -> A.Value)
sosToPSModifiers =
  M.empty

$(enableKVPG ''SosT ['id] [])
