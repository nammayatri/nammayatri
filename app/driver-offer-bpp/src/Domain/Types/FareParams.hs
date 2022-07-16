module Domain.Types.FareParams where

import Beckn.Types.Amount
import Beckn.Utils.GenericPretty (PrettyShow)
import EulerHS.Prelude

data FareParameters = FareParameters
  { fareForPickup :: Amount,
    distanceFare :: Amount,
    driverSelectedFare :: Maybe Amount,
    nightShiftRate :: Maybe Amount,
    nightCoefIncluded :: Bool
  }
  deriving (Generic, Show, Eq, PrettyShow)
