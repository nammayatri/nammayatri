{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyInterCityDetails where

import Data.Aeson
import qualified Domain.Types.FarePolicy.Common
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FarePolicyInterCityDetails = FarePolicyInterCityDetails
  { baseFare :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    deadKmFare :: Kernel.Types.Common.HighPrecMoney,
    defaultWaitTimeAtDestination :: Kernel.Types.Common.Minutes,
    farePolicyId :: Kernel.Prelude.Text,
    kmPerPlannedExtraHour :: Kernel.Types.Common.Kilometers,
    nightShiftCharge :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge,
    perDayMaxAllowanceInMins :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    perDayMaxHourAllowance :: Kernel.Types.Common.Hours,
    perExtraKmRate :: Kernel.Types.Common.HighPrecMoney,
    perExtraMinRate :: Kernel.Types.Common.HighPrecMoney,
    perHourCharge :: Kernel.Types.Common.HighPrecMoney,
    perKmRateOneWay :: Kernel.Types.Common.HighPrecMoney,
    perKmRateRoundTrip :: Kernel.Types.Common.HighPrecMoney,
    stateEntryPermitCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    waitingChargeInfo :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingChargeInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
