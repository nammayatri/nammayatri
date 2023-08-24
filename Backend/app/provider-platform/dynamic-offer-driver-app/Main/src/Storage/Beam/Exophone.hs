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
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Storage.Beam.Exophone where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Exophone as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.ExophoneType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.ExophoneType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.ExophoneType

instance FromBackendRow Postgres Domain.ExophoneType

data ExophoneT f = ExophoneT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    primaryPhone :: B.C f Text,
    backupPhone :: B.C f Text,
    exophoneType :: B.C f Domain.ExophoneType,
    isPrimaryDown :: B.C f Bool,
    callService :: B.C f CallService,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ExophoneT where
  data PrimaryKey ExophoneT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Exophone = ExophoneT Identity

exophoneTMod :: ExophoneT (B.FieldModification (B.TableField ExophoneT))
exophoneTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      primaryPhone = B.fieldNamed "primary_phone",
      backupPhone = B.fieldNamed "backup_phone",
      isPrimaryDown = B.fieldNamed "is_primary_down",
      exophoneType = B.fieldNamed "exophone_type",
      callService = B.fieldNamed "call_service",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''ExophoneT ['id] [['merchantId], ['primaryPhone], ['backupPhone]])

$(mkTableInstances ''ExophoneT "exophone" "atlas_driver_offer_bpp")
