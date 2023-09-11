{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Disability where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data DisabilityT f = DisabilityT
  { id :: B.C f Text,
    tag :: B.C f Text,
    description :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table DisabilityT where
  data PrimaryKey DisabilityT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Disability = DisabilityT Identity

disabilityTMod :: DisabilityT (B.FieldModification (B.TableField DisabilityT))
disabilityTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      tag = B.fieldNamed "tag",
      description = B.fieldNamed "description"
    }

$(enableKVPG ''DisabilityT ['id] [])

$(mkTableInstances ''DisabilityT "disability" "atlas_app")
