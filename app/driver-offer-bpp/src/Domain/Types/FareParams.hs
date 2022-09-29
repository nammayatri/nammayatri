module Domain.Types.FareParams where

import Beckn.Prelude
import Beckn.Types.Common (Money)
import Beckn.Utils.GenericPretty (PrettyShow)

data FareParameters = FareParameters
  { baseFare :: Money,
    extraKmFare :: Maybe Money,
    driverSelectedFare :: Maybe Money,
    nightShiftRate :: Maybe Double,
    nightCoefIncluded :: Bool
  }
  deriving (Generic, Show, Eq, PrettyShow)
