{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FullFarePolicyInterCityDetailsPerKmRateSection where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FullFarePolicyInterCityDetailsPerKmRateSectionT f = FullFarePolicyInterCityDetailsPerKmRateSectionT
  { farePolicyId :: (B.C f Kernel.Prelude.Text),
    isRoundTrip :: (B.C f Kernel.Prelude.Bool),
    perKmRate :: (B.C f Kernel.Types.Common.HighPrecMoney),
    startDistance :: (B.C f Kernel.Types.Common.Meters),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FullFarePolicyInterCityDetailsPerKmRateSectionT where
  data PrimaryKey FullFarePolicyInterCityDetailsPerKmRateSectionT f
    = FullFarePolicyInterCityDetailsPerKmRateSectionId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Bool) (B.C f Kernel.Types.Common.Meters)
    deriving (Generic, B.Beamable)
  primaryKey = FullFarePolicyInterCityDetailsPerKmRateSectionId <$> farePolicyId <*> isRoundTrip <*> startDistance

type FullFarePolicyInterCityDetailsPerKmRateSection = FullFarePolicyInterCityDetailsPerKmRateSectionT Identity

$(enableKVPG (''FullFarePolicyInterCityDetailsPerKmRateSectionT) [('farePolicyId), ('isRoundTrip), ('startDistance)] [[('farePolicyId)]])

$(mkTableInstances (''FullFarePolicyInterCityDetailsPerKmRateSectionT) "fare_policy_inter_city_details_per_km_rate_section")
