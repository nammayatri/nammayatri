{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicySlabsDetailsSlab where

import Data.Aeson
import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FarePolicySlabsDetailsSlab = FarePolicySlabsDetailsSlab
  { baseFare :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    farePolicyId :: Kernel.Prelude.Text,
    id :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    nightShiftCharge :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge,
    platformFeeInfo :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab.PlatformFeeInfo,
    startDistance :: Kernel.Types.Common.Meters,
    waitingChargeInfo :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingChargeInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
