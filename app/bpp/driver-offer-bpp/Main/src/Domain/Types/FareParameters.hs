module Domain.Types.FareParameters where

import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow)

data FareParameters = FareParameters
  { id :: Id FareParameters,
    baseFare :: Money,
    extraKmFare :: Maybe Money,
    driverSelectedFare :: Maybe Money,
    nightShiftRate :: Maybe Centesimal,
    nightCoefIncluded :: Bool,
    waitingChargePerMin :: Maybe Money
  }
  deriving (Generic, Show, Eq, PrettyShow)
