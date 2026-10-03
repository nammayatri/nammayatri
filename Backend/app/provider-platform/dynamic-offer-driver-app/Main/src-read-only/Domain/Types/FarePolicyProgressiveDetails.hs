{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyProgressiveDetails where

import Data.Aeson
import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FarePolicyProgressiveDetails = FarePolicyProgressiveDetails
  { baseDistance :: Kernel.Types.Common.Meters,
    baseFare :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    deadKmFare :: Kernel.Types.Common.HighPrecMoney,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    farePolicyId :: Kernel.Prelude.Text,
    nightShiftCharge :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.NightShiftCharge,
    perMinRateDurationBasis :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.FarePolicyProgressiveDetails.PerMinRateDurationBasis,
    pickupCharges :: Domain.Types.FarePolicy.Common.PickupCharges,
    waitingChargeInfo :: Kernel.Prelude.Maybe Domain.Types.FarePolicy.Common.WaitingChargeInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
