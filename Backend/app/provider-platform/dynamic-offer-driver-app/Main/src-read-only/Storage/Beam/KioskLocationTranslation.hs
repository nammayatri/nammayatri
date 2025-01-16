{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.KioskLocationTranslation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data KioskLocationTranslationT f = KioskLocationTranslationT
  { address :: B.C f Kernel.Prelude.Text,
    kioskLocationId :: B.C f Kernel.Prelude.Text,
    landmark :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.External.Types.Language
  }
  deriving (Generic, B.Beamable)

instance B.Table KioskLocationTranslationT where
  data PrimaryKey KioskLocationTranslationT f = KioskLocationTranslationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = KioskLocationTranslationId . kioskLocationId

type KioskLocationTranslation = KioskLocationTranslationT Identity

$(enableKVPG ''KioskLocationTranslationT ['kioskLocationId] [['language]])

$(mkTableInstances ''KioskLocationTranslationT "kiosk_location_translation")
