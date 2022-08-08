module Domain.Types.FareParams.Internal where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Utils.GenericPretty (PrettyShow)

data FareParameters = FareParameters
  { baseFare :: Amount,
    extraKmFare :: Maybe Amount,
    driverSelectedFare :: Maybe Amount,
    nightShiftRate :: Maybe Amount,
    nightCoefIncluded :: Bool
  }
  deriving (Generic, Show, Eq, PrettyShow)
