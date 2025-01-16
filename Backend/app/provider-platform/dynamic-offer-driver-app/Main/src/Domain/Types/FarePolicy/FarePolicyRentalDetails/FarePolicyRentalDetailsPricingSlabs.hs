{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs where

import Data.Aeson as DA
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Domain.Types.Common
import Kernel.Prelude

data FPRentalDetailsPricingSlabsD (s :: UsageSafety) = FPRentalDetailsPricingSlabs
  { timePercentage :: Int,
    distancePercentage :: Int,
    farePercentage :: Int,
    includeActualTimePercentage :: Bool,
    includeActualDistPercentage :: Bool
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPRentalDetailsPricingSlabs = FPRentalDetailsPricingSlabsD 'Safe

instance FromJSON (FPRentalDetailsPricingSlabsD 'Unsafe)

instance ToJSON (FPRentalDetailsPricingSlabsD 'Unsafe)

instance FromJSON (FPRentalDetailsPricingSlabsD 'Safe)

instance ToJSON (FPRentalDetailsPricingSlabsD 'Safe)

findFPRentalDetailsByTimeAndDistancePercentage :: Int -> Int -> NonEmpty (FPRentalDetailsPricingSlabsD s) -> FPRentalDetailsPricingSlabsD s
findFPRentalDetailsByTimeAndDistancePercentage timePercent distPercent slabList = do
  case NE.filter (\slab -> slab.distancePercentage <= distPercent && slab.timePercentage <= timePercent) $ NE.sortBy (comparing (.timePercentage) <> comparing (.distancePercentage)) slabList of
    [] -> error $ "Slab for time-distance percentage = " <> show timePercent <> "-" <> show distPercent <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPRentalDetailsPricingSlabsAPIEntity = FPRentalDetailsPricingSlabsAPIEntity
  { timePercentage :: Int,
    distancePercentage :: Int,
    farePercentage :: Int,
    includeActualTimePercentage :: Bool,
    includeActualDistPercentage :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeFPRentalDetailsPricingSlabsList :: [FPRentalDetailsPricingSlabsAPIEntity] -> [FPRentalDetailsPricingSlabs]
makeFPRentalDetailsPricingSlabsList = fmap makeFPRentalDetailsPricingSlabs

makeFPRentalDetailsParkingSlabsAPIEntity :: FPRentalDetailsPricingSlabs -> FPRentalDetailsPricingSlabsAPIEntity
makeFPRentalDetailsParkingSlabsAPIEntity FPRentalDetailsPricingSlabs {..} = FPRentalDetailsPricingSlabsAPIEntity {..}

makeFPRentalDetailsPricingSlabs :: FPRentalDetailsPricingSlabsAPIEntity -> FPRentalDetailsPricingSlabs
makeFPRentalDetailsPricingSlabs FPRentalDetailsPricingSlabsAPIEntity {..} = FPRentalDetailsPricingSlabs {..}
