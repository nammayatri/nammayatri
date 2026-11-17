{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FullFarePolicyInterCityDetailsPerKmRateSection = FullFarePolicyInterCityDetailsPerKmRateSection
  { farePolicyId :: Kernel.Prelude.Text,
    isRoundTrip :: Kernel.Prelude.Bool,
    perKmRate :: Kernel.Types.Common.HighPrecMoney,
    startDistance :: Kernel.Types.Common.Meters,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
