module Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Common (Meters)
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data PerExtraKmRate = PerExtraKmRate
  { distanceRangeStart :: Meters,
    fare :: Amount
  }
  deriving (Generic, Show, Eq)

data PerExtraKmRateAPIEntity = PerExtraKmRateAPIEntity
  { distanceRangeStart :: Meters,
    fare :: Double
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makePerExtraKmRateAPIEntity :: PerExtraKmRate -> PerExtraKmRateAPIEntity
makePerExtraKmRateAPIEntity PerExtraKmRate {..} =
  PerExtraKmRateAPIEntity
    { distanceRangeStart = distanceRangeStart,
      fare = realToFrac fare
    }

fromPerExtraKmRateAPIEntity :: PerExtraKmRateAPIEntity -> PerExtraKmRate
fromPerExtraKmRateAPIEntity PerExtraKmRateAPIEntity {..} = do
  PerExtraKmRate
    { distanceRangeStart = distanceRangeStart,
      fare = realToFrac fare
    }

validatePerExtraKmRateAPIEntity :: Validate PerExtraKmRateAPIEntity
validatePerExtraKmRateAPIEntity extraKmRate =
  sequenceA_
    [ validateField "fare" extraKmRate.fare $ InRange @Double 1 99,
      validateField "distanceRangeStart" extraKmRate.distanceRangeStart $ Min @Meters 0
    ]
