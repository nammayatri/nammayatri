{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyInterCityDetailsPricingSlabs where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FarePolicyInterCityDetailsPricingSlabsT f = FarePolicyInterCityDetailsPricingSlabsT
  { distancePercentage :: (B.C f Kernel.Prelude.Int),
    farePercentage :: (B.C f Kernel.Prelude.Int),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    includeActualDistPercentage :: (B.C f Kernel.Prelude.Bool),
    includeActualTimePercentage :: (B.C f Kernel.Prelude.Bool),
    timePercentage :: (B.C f Kernel.Prelude.Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyInterCityDetailsPricingSlabsT where
  data PrimaryKey FarePolicyInterCityDetailsPricingSlabsT f
    = FarePolicyInterCityDetailsPricingSlabsId (B.C f Kernel.Prelude.Int) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int)
    deriving (Generic, B.Beamable)
  primaryKey = FarePolicyInterCityDetailsPricingSlabsId <$> distancePercentage <*> farePolicyId <*> timePercentage

type FarePolicyInterCityDetailsPricingSlabs = FarePolicyInterCityDetailsPricingSlabsT Identity

$(enableKVPG (''FarePolicyInterCityDetailsPricingSlabsT) [('distancePercentage), ('farePolicyId), ('timePercentage)] [])

$(mkTableInstances (''FarePolicyInterCityDetailsPricingSlabsT) "fare_policy_inter_city_details_pricing_slabs")
