 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
