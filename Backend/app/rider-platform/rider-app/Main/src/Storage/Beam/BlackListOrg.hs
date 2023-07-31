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

module Storage.Beam.BlackListOrg where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.BlackListOrg as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.BlackListOrgType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.BlackListOrgType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.BlackListOrgType

instance FromBackendRow Postgres Domain.BlackListOrgType

instance IsString Domain.BlackListOrgType where
  fromString = show

data BlackListOrgT f = BlackListOrgT
  { id :: B.C f Text,
    subscriberId :: B.C f Text,
    orgType :: B.C f Domain.BlackListOrgType
  }
  deriving (Generic, B.Beamable)

instance B.Table BlackListOrgT where
  data PrimaryKey BlackListOrgT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type BlackListOrg = BlackListOrgT Identity

deriving stock instance Ord Domain.BlackListOrgType

blackListOrgTMod :: BlackListOrgT (B.FieldModification (B.TableField BlackListOrgT))
blackListOrgTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      subscriberId = B.fieldNamed "subscriber_id",
      orgType = B.fieldNamed "type"
    }

$(enableKVPG ''BlackListOrgT ['id] [['subscriberId]])

$(mkTableInstances ''BlackListOrgT "black_list_org" "atlas_app")
