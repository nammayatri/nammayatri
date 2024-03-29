{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.DisabilityTranslation where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

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

$(enableKVPG ''DisabilityTranslationT ['disabilityId, 'language] [])

$(mkTableInstances ''DisabilityTranslationT "disability_translation")
