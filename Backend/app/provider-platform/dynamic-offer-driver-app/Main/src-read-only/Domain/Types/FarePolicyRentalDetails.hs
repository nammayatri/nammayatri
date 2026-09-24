{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyRentalDetails where

import Data.Aeson
import qualified Domain.Types.FarePolicy.Common
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FarePolicyRentalDetails = FarePolicyRentalDetails
  { baseFare :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    deadKmFare :: Kernel.Types.Common.HighPrecMoney,
    farePolicyId :: Kernel.Prelude.Text,
    includedKmPerHr :: Kernel.Types.Common.Kilometers,
    maxAdditionalKmsLimit :: Kernel.Types.Common.Kilometers,
    nightShiftCharge :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge,
    perExtraKmRate :: Kernel.Types.Common.HighPrecMoney,
    perExtraMinRate :: Kernel.Types.Common.HighPrecMoney,
    perHourCharge :: Kernel.Types.Common.HighPrecMoney,
    plannedPerKmRate :: Kernel.Types.Common.HighPrecMoney,
    totalAdditionalKmsLimit :: Kernel.Types.Common.Kilometers,
    waitingChargeInfo :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingChargeInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
