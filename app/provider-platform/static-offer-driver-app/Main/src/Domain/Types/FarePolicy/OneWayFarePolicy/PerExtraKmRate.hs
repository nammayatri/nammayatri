module Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate where

import Data.OpenApi (ToSchema)
import Domain.Types.Common
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common (HighPrecMoney, Meters)
import Kernel.Types.Predicate
import Kernel.Utils.Validation

data PerExtraKmRateD (s :: UsageSafety) = PerExtraKmRate
  { distanceRangeStart :: Meters,
    fare :: HighPrecMoney
  }
  deriving (Generic, Show, Eq)

type PerExtraKmRate = PerExtraKmRateD 'Safe

instance FromJSON (PerExtraKmRateD 'Unsafe)

instance ToJSON (PerExtraKmRateD 'Unsafe)

data PerExtraKmRateAPIEntity = PerExtraKmRateAPIEntity
  { distanceRangeStart :: Meters,
    fare :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makePerExtraKmRateAPIEntity :: PerExtraKmRate -> PerExtraKmRateAPIEntity
makePerExtraKmRateAPIEntity PerExtraKmRate {..} =
  PerExtraKmRateAPIEntity
    { ..
    }

fromPerExtraKmRateAPIEntity :: PerExtraKmRateAPIEntity -> PerExtraKmRate
fromPerExtraKmRateAPIEntity PerExtraKmRateAPIEntity {..} = do
  PerExtraKmRate
    { ..
    }

validatePerExtraKmRateAPIEntity :: Validate PerExtraKmRateAPIEntity
validatePerExtraKmRateAPIEntity extraKmRate =
  sequenceA_
    [ validateField "fare" extraKmRate.fare $ InRange @HighPrecMoney 1 99,
      validateField "distanceRangeStart" extraKmRate.distanceRangeStart $ Min @Meters 0
    ]
