{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.KioskLocationTranslation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Types
import qualified Database.Beam as B



data KioskLocationTranslationT f
    = KioskLocationTranslationT {address :: (B.C f Kernel.Prelude.Text),
                                 kioskLocationId :: (B.C f Kernel.Prelude.Text),
                                 landmark :: (B.C f Kernel.Prelude.Text),
                                 language :: (B.C f Kernel.External.Types.Language)}
    deriving (Generic, B.Beamable)
instance B.Table KioskLocationTranslationT
    where data PrimaryKey KioskLocationTranslationT f = KioskLocationTranslationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = KioskLocationTranslationId . kioskLocationId
type KioskLocationTranslation = KioskLocationTranslationT Identity

$(enableKVPG (''KioskLocationTranslationT) [('kioskLocationId)] [[('language)]])

$(mkTableInstances (''KioskLocationTranslationT) "kiosk_location_translation")

