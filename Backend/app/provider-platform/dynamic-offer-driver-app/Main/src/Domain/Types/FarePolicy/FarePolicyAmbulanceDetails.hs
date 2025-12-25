{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyAmbulanceDetails where

import Data.Aeson as DA
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Domain.Types.Common
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Domain
import qualified Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as Domain
import Kernel.Prelude
import Kernel.Types.Common

newtype FPAmbulanceDetailsD (s :: UsageSafety) = FPAmbulanceDetails
  { slabs :: NonEmpty (FPAmbulanceDetailsSlabD s)
  }
  deriving (Generic, Show, Eq, ToSchema)

data FPAmbulanceDetailsSlabD (s :: UsageSafety) = FPAmbulanceDetailsSlab
  { id :: Int,
    baseFare :: HighPrecMoney,
    baseDistance :: Meters,
    vehicleAge :: Months,
    perKmRate :: HighPrecMoney,
    currency :: Currency,
    waitingChargeInfo :: Maybe Domain.WaitingChargeInfo,
    platformFeeInfo :: Maybe Domain.PlatformFeeInfo,
    nightShiftCharge :: Maybe Domain.NightShiftCharge
  }
  deriving (Generic, Show, Eq, ToSchema)

findFPAmbulanceDetailsSlabByAge :: Months -> NonEmpty (FPAmbulanceDetailsSlabD s) -> FPAmbulanceDetailsSlabD s
findFPAmbulanceDetailsSlabByAge age slabList = do
  case NE.filter (\slab -> slab.vehicleAge <= age) $ NE.sortBy (comparing (.vehicleAge)) slabList of
    [] -> error $ "Slab for age = " <> show age <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

type FPAmbulanceDetailsSlab = FPAmbulanceDetailsSlabD 'Safe

instance FromJSON (FPAmbulanceDetailsD 'Unsafe)

instance ToJSON (FPAmbulanceDetailsD 'Unsafe)

instance FromJSON (FPAmbulanceDetailsD 'Safe)

instance ToJSON (FPAmbulanceDetailsD 'Safe)

instance FromJSON (FPAmbulanceDetailsSlabD 'Unsafe)

instance ToJSON (FPAmbulanceDetailsSlabD 'Unsafe)

instance FromJSON (FPAmbulanceDetailsSlabD 'Safe)

instance ToJSON (FPAmbulanceDetailsSlabD 'Safe)
