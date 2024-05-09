{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FullFarePolicyProgressiveDetailsPerExtraMinRateSection where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT f = FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT
  { farePolicyId :: (B.C f Kernel.Prelude.Text),
    perExtraMinRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
    startMin :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT where
  data PrimaryKey FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT f = FullFarePolicyProgressiveDetailsPerExtraMinRateSectionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FullFarePolicyProgressiveDetailsPerExtraMinRateSectionId . farePolicyId

type FullFarePolicyProgressiveDetailsPerExtraMinRateSection = FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT Identity

$(enableKVPG (''FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT) [('farePolicyId)] [])

$(mkTableInstances (''FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT) "fare_policy_progressive_details_per_extra_min_rate_section")
