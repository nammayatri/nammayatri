module Domain.Types.FarePolicy.PerExtraKmRate where

import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data PerExtraKmRate = PerExtraKmRate
  { distanceRangeStart :: Rational,
    fare :: Rational
  }
  deriving (Generic, Show, Eq)

data PerExtraKmRateAPIEntity = PerExtraKmRateAPIEntity
  { distanceRangeStart :: Double,
    fare :: Double
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makePerExtraKmRateAPIEntity :: PerExtraKmRate -> PerExtraKmRateAPIEntity
makePerExtraKmRateAPIEntity PerExtraKmRate {..} =
  PerExtraKmRateAPIEntity
    { distanceRangeStart = fromRational distanceRangeStart,
      fare = fromRational fare
    }

fromPerExtraKmRateAPIEntity :: PerExtraKmRateAPIEntity -> PerExtraKmRate
fromPerExtraKmRateAPIEntity PerExtraKmRateAPIEntity {..} = do
  PerExtraKmRate
    { distanceRangeStart = toRational distanceRangeStart,
      fare = toRational fare
    }

validatePerExtraKmRateAPIEntity :: Validate PerExtraKmRateAPIEntity
validatePerExtraKmRateAPIEntity extraKmRate =
  sequenceA_
    [ validateField "fare" extraKmRate.fare $ InRange @Double 1 99,
      validateField "distanceRangeStart" extraKmRate.distanceRangeStart $ Min @Double 0
    ]
