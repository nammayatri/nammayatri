module Domain.Types.FareParameters where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty (PrettyShow)

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
