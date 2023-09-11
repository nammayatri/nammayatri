{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.DisabilityTranslation where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data DisabilityTranslationT f = DisabilityTranslationT
  { disabilityId :: B.C f Text,
    disabilityTag :: B.C f Text,
    translation :: B.C f Text,
    language :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table DisabilityTranslationT where
  data PrimaryKey DisabilityTranslationT f
    = CompositeKey (B.C f Text) (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey dt = CompositeKey (disabilityId dt) (language dt)

type DisabilityTranslation = DisabilityTranslationT Identity

disabilityTranslationTMod :: DisabilityTranslationT (B.FieldModification (B.TableField DisabilityTranslationT))
disabilityTranslationTMod =
  B.tableModification
    { disabilityId = B.fieldNamed "disability_id",
      disabilityTag = B.fieldNamed "disability_tag",
      translation = B.fieldNamed "translation",
      language = B.fieldNamed "language"
    }

$(enableKVPG ''DisabilityTranslationT ['disabilityId, 'language] [])

$(mkTableInstances ''DisabilityTranslationT "disability_translation" "atlas_app")
