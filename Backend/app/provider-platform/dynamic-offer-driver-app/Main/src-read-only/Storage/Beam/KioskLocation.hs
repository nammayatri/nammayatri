{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.KioskLocation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data KioskLocationT f
    = KioskLocationT {address :: (B.C f Kernel.Prelude.Text),
                      contact :: (B.C f Kernel.Prelude.Text),
                      id :: (B.C f Kernel.Prelude.Text),
                      landmark :: (B.C f Kernel.Prelude.Text),
                      latitude :: (B.C f Kernel.Prelude.Double),
                      longitude :: (B.C f Kernel.Prelude.Double),
                      merchantId :: (B.C f Kernel.Prelude.Text)}
    deriving (Generic, B.Beamable)
instance B.Table KioskLocationT
    where data PrimaryKey KioskLocationT f = KioskLocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = KioskLocationId . id
type KioskLocation = KioskLocationT Identity

$(enableKVPG (''KioskLocationT) [('id)] [])

$(mkTableInstances (''KioskLocationT) "kiosk_location")

