{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FarePolicyProgressiveDetailsPerExtraKmRateSection = FarePolicyProgressiveDetailsPerExtraKmRateSection
  { baseFareDepreciation :: Kernel.Types.Common.HighPrecMoney,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    farePolicyId :: Kernel.Prelude.Text,
    perExtraKmRate :: Kernel.Types.Common.HighPrecMoney,
    startDistance :: Kernel.Types.Common.Meters
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
