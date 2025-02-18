{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DisabilityTranslation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DisabilityTranslationT f = DisabilityTranslationT
  { disabilityId :: B.C f Kernel.Prelude.Text,
    disabilityTag :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.Prelude.Text,
    translation :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table DisabilityTranslationT where
  data PrimaryKey DisabilityTranslationT f = DisabilityTranslationId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DisabilityTranslationId <$> disabilityId <*> language

type DisabilityTranslation = DisabilityTranslationT Identity

$(enableKVPG ''DisabilityTranslationT ['disabilityId, 'language] [])

$(mkTableInstances ''DisabilityTranslationT "disability_translation")
