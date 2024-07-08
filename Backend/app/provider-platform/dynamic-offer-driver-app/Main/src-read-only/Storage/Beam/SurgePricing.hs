{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SurgePricing where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SurgePricingT f = SurgePricingT
  { dayOfWeek :: (B.C f Kernel.Prelude.Text),
    hourOfDay :: (B.C f Kernel.Prelude.Int),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    sourceHex :: (B.C f Kernel.Prelude.Text),
    surgeMultiplier :: (B.C f Kernel.Prelude.Double),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SurgePricingT where
  data PrimaryKey SurgePricingT f = SurgePricingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SurgePricingId . id

type SurgePricing = SurgePricingT Identity

$(enableKVPG (''SurgePricingT) [('id)] [])

$(mkTableInstances (''SurgePricingT) "surge_pricing")
