{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.DisabilityTranslation where

import qualified Database.Beam as B
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

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

$(TH.enableKVPG ''DisabilityTranslationT ['disabilityId, 'language] [])

$(TH.mkTableInstances ''DisabilityTranslationT "disability_translation")
