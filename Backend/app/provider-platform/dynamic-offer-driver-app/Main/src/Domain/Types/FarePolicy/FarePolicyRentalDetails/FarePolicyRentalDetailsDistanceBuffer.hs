{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffer where

import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Domain.Types.Common
import Kernel.Prelude

data FPRentalDetailsDistanceBuffersD (s :: UsageSafety) = FPRentalDetailsDistanceBuffers
  { rideDuration :: Int,
    bufferKms :: Int
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPRentalDetailsDistanceBuffers = FPRentalDetailsDistanceBuffersD 'Safe

instance FromJSON (FPRentalDetailsDistanceBuffersD 'Unsafe)

instance ToJSON (FPRentalDetailsDistanceBuffersD 'Unsafe)

findFPRentalDetailsByDuration :: Int -> NonEmpty (FPRentalDetailsDistanceBuffersD s) -> FPRentalDetailsDistanceBuffersD s
findFPRentalDetailsByDuration duration slabList = do
  case NE.filter (\slab -> slab.rideDuration <= duration) $ NE.sortBy (comparing (.rideDuration)) slabList of
    [] -> error $ "Slab for duration = " <> show duration <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPRentalDetailsDistanceBuffersAPIEntity = FPRentalDetailsDistanceBuffersAPIEntity
  { rideDuration :: Int,
    bufferKms :: Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeFPRentalDetailsDistanceBuffersAPIEntity :: FPRentalDetailsDistanceBuffers -> FPRentalDetailsDistanceBuffersAPIEntity
makeFPRentalDetailsDistanceBuffersAPIEntity FPRentalDetailsDistanceBuffers {..} =
  FPRentalDetailsDistanceBuffersAPIEntity
    { rideDuration = rideDuration,
      bufferKms = bufferKms
    }
