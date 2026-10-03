{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyInterCityDetailsPricingSlabs where

import Data.Aeson
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data FarePolicyInterCityDetailsPricingSlabs = FarePolicyInterCityDetailsPricingSlabs
  { distancePercentage :: Kernel.Prelude.Int,
    farePercentage :: Kernel.Prelude.Int,
    farePolicyId :: Kernel.Prelude.Text,
    includeActualDistPercentage :: Kernel.Prelude.Bool,
    includeActualTimePercentage :: Kernel.Prelude.Bool,
    timePercentage :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
