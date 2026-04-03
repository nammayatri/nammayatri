{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.SurgePricing where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Domain.Types.Common
import qualified Domain.Types.UtilsTH
import qualified Database.Beam as B



data SurgePricingT f
    = SurgePricingT {dayOfWeek :: (B.C f Kernel.Prelude.Text),
                     hourOfDay :: (B.C f Kernel.Prelude.Int),
                     id :: (B.C f Kernel.Prelude.Text),
                     sourceHex :: (B.C f Kernel.Prelude.Text),
                     surgeMultiplier :: (B.C f Kernel.Types.Common.Centesimal),
                     vehicleServiceTier :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType)),
                     createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table SurgePricingT
    where data PrimaryKey SurgePricingT f = SurgePricingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = SurgePricingId . id
type SurgePricing = SurgePricingT Identity

$(enableKVPG (''SurgePricingT) [('id)] [])

$(mkTableInstances (''SurgePricingT) "surge_pricing")

$(Domain.Types.UtilsTH.mkCacParseInstance (''SurgePricingT))

