{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyAmbulanceDetailsSlab where

import Data.Aeson
import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FarePolicyAmbulanceDetailsSlab = FarePolicyAmbulanceDetailsSlab
  { baseDistance :: Kernel.Types.Common.Meters,
    baseFare :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    farePolicyId :: Kernel.Prelude.Text,
    id :: Kernel.Prelude.Int,
    nightShiftCharge :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge,
    perKmRate :: Kernel.Types.Common.HighPrecMoney,
    platformFeeInfo :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab.PlatformFeeInfo,
    vehicleAge :: Kernel.Types.Common.Months,
    waitingChargeInfo :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingChargeInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
