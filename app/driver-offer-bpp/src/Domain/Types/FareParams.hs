module Domain.Types.FareParams where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Utils.GenericPretty (PrettyShow)

data FareParameters = FareParameters
  { fareForPickup :: Amount,
    distanceFare :: Amount,
    driverSelectedFare :: Maybe Amount,
    nightShiftRate :: Maybe Amount,
    nightCoefIncluded :: Bool
  }
  deriving (Generic, Show, Eq, PrettyShow)
