{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs where

import Data.Aeson as DA
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Domain.Types.Common
import Kernel.Prelude

data FPInterCityDetailsPricingSlabsD (s :: UsageSafety) = FPInterCityDetailsPricingSlabs
  { timePercentage :: Int,
    distancePercentage :: Int,
    farePercentage :: Int,
    includeActualTimePercentage :: Bool,
    includeActualDistPercentage :: Bool
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPInterCityDetailsPricingSlabs = FPInterCityDetailsPricingSlabsD 'Safe

instance FromJSON (FPInterCityDetailsPricingSlabsD 'Unsafe)

instance ToJSON (FPInterCityDetailsPricingSlabsD 'Unsafe)

instance FromJSON (FPInterCityDetailsPricingSlabsD 'Safe)

instance ToJSON (FPInterCityDetailsPricingSlabsD 'Safe)

findFPInterCityDetailsByTimeAndDistancePercentage :: Int -> Int -> NonEmpty (FPInterCityDetailsPricingSlabsD s) -> FPInterCityDetailsPricingSlabsD s
findFPInterCityDetailsByTimeAndDistancePercentage timePercent distPercent slabList = do
  case NE.filter (\slab -> slab.distancePercentage <= distPercent && slab.timePercentage <= timePercent) $ NE.sortBy (comparing (.timePercentage) <> comparing (.distancePercentage)) slabList of
    [] -> error $ "Slab for time-distance percentage = " <> show timePercent <> "-" <> show distPercent <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPInterCityDetailsPricingSlabsAPIEntity = FPInterCityDetailsPricingSlabsAPIEntity
  { timePercentage :: Int,
    distancePercentage :: Int,
    farePercentage :: Int,
    includeActualTimePercentage :: Bool,
    includeActualDistPercentage :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeFPInterCityDetailsPricingSlabsList :: [FPInterCityDetailsPricingSlabsAPIEntity] -> [FPInterCityDetailsPricingSlabs]
makeFPInterCityDetailsPricingSlabsList = fmap makeFPInterCityDetailsPricingSlabs

makeFPInterCityDetailsParkingSlabsAPIEntity :: FPInterCityDetailsPricingSlabs -> FPInterCityDetailsPricingSlabsAPIEntity
makeFPInterCityDetailsParkingSlabsAPIEntity FPInterCityDetailsPricingSlabs {..} = FPInterCityDetailsPricingSlabsAPIEntity {..}

makeFPInterCityDetailsPricingSlabs :: FPInterCityDetailsPricingSlabsAPIEntity -> FPInterCityDetailsPricingSlabs
makeFPInterCityDetailsPricingSlabs FPInterCityDetailsPricingSlabsAPIEntity {..} = FPInterCityDetailsPricingSlabs {..}
