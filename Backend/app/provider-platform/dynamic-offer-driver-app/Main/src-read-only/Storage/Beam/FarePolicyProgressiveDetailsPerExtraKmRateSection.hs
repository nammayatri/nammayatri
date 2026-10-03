{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FarePolicyProgressiveDetailsPerExtraKmRateSectionT f = FarePolicyProgressiveDetailsPerExtraKmRateSectionT
  { baseFareDepreciation :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    perExtraKmRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
    startDistance :: (B.C f Kernel.Types.Common.Meters)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyProgressiveDetailsPerExtraKmRateSectionT where
  data PrimaryKey FarePolicyProgressiveDetailsPerExtraKmRateSectionT f = FarePolicyProgressiveDetailsPerExtraKmRateSectionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyProgressiveDetailsPerExtraKmRateSectionId . farePolicyId

type FarePolicyProgressiveDetailsPerExtraKmRateSection = FarePolicyProgressiveDetailsPerExtraKmRateSectionT Identity

$(enableKVPG (''FarePolicyProgressiveDetailsPerExtraKmRateSectionT) [('farePolicyId)] [])

$(mkTableInstances (''FarePolicyProgressiveDetailsPerExtraKmRateSectionT) "fare_policy_progressive_details_per_extra_km_rate_section")
