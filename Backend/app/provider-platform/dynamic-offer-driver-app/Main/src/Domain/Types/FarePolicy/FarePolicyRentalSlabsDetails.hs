{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyRentalSlabsDetails
  ( 
    module Reexport,
    module Domain.Types.FarePolicy.FarePolicyRentalSlabsDetails,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Ord
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Types.Common

-- import Kernel.Types.Common
import Domain.Types.FarePolicy.FarePolicyRentalSlabDetails.FarePolicyRentalSlabDetails as Reexport

newtype FPRSlabsDetailsD (s :: UsageSafety) = FPRSlabsDetails
  { rentalSlabs :: NonEmpty (FPRSlabDetailsSlabD s)
  }
  deriving (Generic, Show, Eq)

type FPRSlabsDetails = FPRSlabsDetailsD 'Safe

instance FromJSON (FPRSlabsDetailsD 'Unsafe)

instance ToJSON (FPRSlabsDetailsD 'Unsafe)

findFPRSlabsDetailsSlabByDistance :: Meters -> NonEmpty (FPRSlabDetailsSlabD s) -> FPRSlabDetailsSlabD s
findFPRSlabsDetailsSlabByDistance dist slabList = do
  case NE.filter (\slab -> slab.baseDistance <= metersToKilometers dist) $ NE.sortBy (comparing (.baseDistance)) slabList of
    [] -> error $ "Slab for dist = " <> show dist <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a
-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

newtype FPRSlabsDetailsAPIEntity = FPRSlabsDetailsAPIEntity
  { 
    rentalSlabs  :: NonEmpty FPRSlabDetailsSlabAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFPRSlabsDetailsAPIEntity :: FPRSlabsDetails -> FPRSlabsDetailsAPIEntity
makeFPRSlabsDetailsAPIEntity FPRSlabsDetails {..} =
  FPRSlabsDetailsAPIEntity
    { rentalSlabs = makeFPRSlabDetailsSlabAPIEntity <$> rentalSlabs
    }
