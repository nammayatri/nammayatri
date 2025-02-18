{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FullFarePolicyProgressiveDetailsPerMinRateSection where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FullFarePolicyProgressiveDetailsPerMinRateSectionT f = FullFarePolicyProgressiveDetailsPerMinRateSectionT
  { currency :: B.C f Kernel.Types.Common.Currency,
    farePolicyId :: B.C f Kernel.Prelude.Text,
    perMinRate :: B.C f Kernel.Types.Common.HighPrecMoney,
    rideDurationInMin :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FullFarePolicyProgressiveDetailsPerMinRateSectionT where
  data PrimaryKey FullFarePolicyProgressiveDetailsPerMinRateSectionT f
    = FullFarePolicyProgressiveDetailsPerMinRateSectionId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int)
    deriving (Generic, B.Beamable)
  primaryKey = FullFarePolicyProgressiveDetailsPerMinRateSectionId <$> farePolicyId <*> rideDurationInMin

type FullFarePolicyProgressiveDetailsPerMinRateSection = FullFarePolicyProgressiveDetailsPerMinRateSectionT Identity

$(enableKVPG ''FullFarePolicyProgressiveDetailsPerMinRateSectionT ['farePolicyId, 'rideDurationInMin] [])

$(mkTableInstances ''FullFarePolicyProgressiveDetailsPerMinRateSectionT "fare_policy_progressive_details_per_min_rate_section")
