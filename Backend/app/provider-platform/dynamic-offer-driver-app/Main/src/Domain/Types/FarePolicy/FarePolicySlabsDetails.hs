{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicySlabsDetails
  ( module Reexport,
    module Domain.Types.FarePolicy.FarePolicySlabsDetails,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Ord
import Domain.Types.Common
import Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as Reexport
import Kernel.Prelude
import Kernel.Types.Common

newtype FPSlabsDetailsD (s :: UsageSafety) = FPSlabsDetails
  { slabs :: NonEmpty (FPSlabsDetailsSlabD s)
  }
  deriving (Generic, Show, Eq)

type FPSlabsDetails = FPSlabsDetailsD 'Safe

instance FromJSON (FPSlabsDetailsD 'Unsafe)

instance ToJSON (FPSlabsDetailsD 'Unsafe)

findFPSlabsDetailsSlabByDistance :: Meters -> NonEmpty (FPSlabsDetailsSlabD s) -> FPSlabsDetailsSlabD s
findFPSlabsDetailsSlabByDistance dist slabList = do
  case NE.filter (\slab -> slab.startDistance < dist) $ NE.sortBy (comparing (.startDistance)) slabList of
    [] -> error $ "Slab for dist = " <> show dist <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

newtype FPSlabsDetailsAPIEntity = FPSlabsDetailsAPIEntity
  { slabs :: NonEmpty FPSlabsDetailsSlabAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFPSlabsDetailsAPIEntity :: FPSlabsDetails -> FPSlabsDetailsAPIEntity
makeFPSlabsDetailsAPIEntity FPSlabsDetails {..} =
  FPSlabsDetailsAPIEntity
    { slabs = makeFPSlabsDetailsSlabAPIEntity <$> slabs
    }
