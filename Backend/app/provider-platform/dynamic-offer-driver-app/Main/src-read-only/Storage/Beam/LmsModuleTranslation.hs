{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LmsModuleTranslation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data LmsModuleTranslationT f = LmsModuleTranslationT
  { description :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.External.Types.Language,
    moduleId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    thumbnailImage :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table LmsModuleTranslationT where
  data PrimaryKey LmsModuleTranslationT f = LmsModuleTranslationId (B.C f Kernel.External.Types.Language) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = LmsModuleTranslationId <$> language <*> moduleId

type LmsModuleTranslation = LmsModuleTranslationT Identity

$(enableKVPG ''LmsModuleTranslationT ['language, 'moduleId] [])

$(mkTableInstances ''LmsModuleTranslationT "lms_module_translation")
