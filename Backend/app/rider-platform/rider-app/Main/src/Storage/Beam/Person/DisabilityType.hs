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

module Storage.Beam.Person.DisabilityType where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data DisabilityTypeT f = DisabilityTypeT
  { id :: B.C f Text,
    tag :: B.C f Text,
    subtag :: B.C f Text,
    description :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DisabilityTypeT where
  data PrimaryKey DisabilityTypeT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DisabilityType = DisabilityTypeT Identity

disabilityTypeTMod :: DisabilityTypeT (B.FieldModification (B.TableField DisabilityTypeT))
disabilityTypeTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      tag = B.fieldNamed "tag",
      subtag = B.fieldNamed "subtag",
      description = B.fieldNamed "description",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''DisabilityTypeT ['id] [])

$(mkTableInstances ''DisabilityTypeT "disability_type" "atlas_app")
