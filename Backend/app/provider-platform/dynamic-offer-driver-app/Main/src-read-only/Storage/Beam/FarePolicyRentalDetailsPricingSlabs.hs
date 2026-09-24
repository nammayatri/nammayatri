{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyRentalDetailsPricingSlabs where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FarePolicyRentalDetailsPricingSlabsT f = FarePolicyRentalDetailsPricingSlabsT
  { distancePercentage :: (B.C f Kernel.Prelude.Int),
    farePercentage :: (B.C f Kernel.Prelude.Int),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    includeActualDistPercentage :: (B.C f Kernel.Prelude.Bool),
    includeActualTimePercentage :: (B.C f Kernel.Prelude.Bool),
    timePercentage :: (B.C f Kernel.Prelude.Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalDetailsPricingSlabsT where
  data PrimaryKey FarePolicyRentalDetailsPricingSlabsT f
    = FarePolicyRentalDetailsPricingSlabsId (B.C f Kernel.Prelude.Int) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int)
    deriving (Generic, B.Beamable)
  primaryKey = FarePolicyRentalDetailsPricingSlabsId <$> distancePercentage <*> farePolicyId <*> timePercentage

type FarePolicyRentalDetailsPricingSlabs = FarePolicyRentalDetailsPricingSlabsT Identity

$(enableKVPG (''FarePolicyRentalDetailsPricingSlabsT) [('distancePercentage), ('farePolicyId), ('timePercentage)] [])

$(mkTableInstances (''FarePolicyRentalDetailsPricingSlabsT) "fare_policy_rental_details_pricing_slabs")
