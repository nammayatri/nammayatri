{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection where

import Data.Aeson as DA
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Types.Common

data FPProgressiveDetailsPerExtraKmRateSectionD (s :: UsageSafety) = FPProgressiveDetailsPerExtraKmRateSection
  { startDistance :: Meters,
    distanceUnit :: DistanceUnit,
    perExtraKmRate :: HighPrecMoney,
    baseFareDepreciation :: HighPrecMoney
  }
  deriving (Generic, Show, Eq)

type FPProgressiveDetailsPerExtraKmRateSection = FPProgressiveDetailsPerExtraKmRateSectionD 'Safe

instance FromJSON (FPProgressiveDetailsPerExtraKmRateSectionD 'Unsafe)

instance ToJSON (FPProgressiveDetailsPerExtraKmRateSectionD 'Unsafe)

instance FromJSON (FPProgressiveDetailsPerExtraKmRateSectionD 'Safe)

instance ToJSON (FPProgressiveDetailsPerExtraKmRateSectionD 'Safe)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPProgressiveDetailsPerExtraKmRateSectionAPIEntity = FPProgressiveDetailsPerExtraKmRateSectionAPIEntity
  { startDistance :: Meters,
    startDistanceWithUnit :: Distance,
    perExtraKmRate :: HighPrecMoney,
    baseFareDepreciation :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeFPProgressiveDetailsPerExtraKmRateSectionAPIEntity :: FPProgressiveDetailsPerExtraKmRateSection -> FPProgressiveDetailsPerExtraKmRateSectionAPIEntity
makeFPProgressiveDetailsPerExtraKmRateSectionAPIEntity FPProgressiveDetailsPerExtraKmRateSection {..} =
  FPProgressiveDetailsPerExtraKmRateSectionAPIEntity
    { startDistanceWithUnit = convertMetersToDistance distanceUnit startDistance,
      ..
    }
