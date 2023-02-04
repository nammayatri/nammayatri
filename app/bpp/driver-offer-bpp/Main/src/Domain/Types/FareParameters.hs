module Domain.Types.FareParameters where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty (PrettyShow)

data FareParameters = FareParameters
  { baseFare :: Money,
    extraKmFare :: Maybe Money,
    driverSelectedFare :: Maybe Money,
    nightShiftRate :: Maybe Centesimal,
    nightCoefIncluded :: Bool,
    waitingChargePerMin :: Maybe Money
  }
  deriving (Generic, Show, Eq, PrettyShow)

-- We need type with id for tabular instances.
-- This logic need to be refactored after merging [BKN-2191] Factor out fare computations to the common lib
data FareParameters' = FareParameters'
  { id :: Id FareParameters,
    baseFare :: Money,
    extraKmFare :: Maybe Money,
    driverSelectedFare :: Maybe Money,
    nightShiftRate :: Maybe Centesimal,
    nightCoefIncluded :: Bool,
    waitingChargePerMin :: Maybe Money
  }
  deriving (Generic, Show, PrettyShow)

buildFareParameters' :: MonadGuid m => FareParameters -> m FareParameters'
buildFareParameters' FareParameters {..} = do
  id <- generateGUID
  pure FareParameters' {..}
