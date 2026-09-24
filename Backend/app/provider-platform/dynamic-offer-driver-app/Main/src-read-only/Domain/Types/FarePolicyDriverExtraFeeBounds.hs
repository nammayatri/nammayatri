{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyDriverExtraFeeBounds where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FarePolicyDriverExtraFeeBounds = FarePolicyDriverExtraFeeBounds
  { defaultStepFee :: Kernel.Types.Common.HighPrecMoney,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    farePolicyId :: Kernel.Prelude.Text,
    id :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxFee :: Kernel.Types.Common.HighPrecMoney,
    minFee :: Kernel.Types.Common.HighPrecMoney,
    startDistance :: Kernel.Types.Common.Meters,
    stepFee :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
