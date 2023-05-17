{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.CustomerExtraFeeBounds where

import qualified Data.List.NonEmpty as NE
import Data.Ord
import Kernel.Prelude
import Kernel.Types.Common

data CustomerExtraFeeBounds = CustomerExtraFeeBounds
  { startDistance :: Meters,
    minFee :: Money,
    maxFee :: Money
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

findFPCustomerExtraFeeBoundsByDistance :: Meters -> NonEmpty CustomerExtraFeeBounds -> CustomerExtraFeeBounds
findFPCustomerExtraFeeBoundsByDistance dist customerExtraFeeBoundsList = do
  case NE.filter (\customerExtraFeeBounds -> customerExtraFeeBounds.startDistance < dist) $ NE.sortBy (comparing (.startDistance)) customerExtraFeeBoundsList of
    [] -> error $ "CustomerExtraFeeBounds for dist = " <> show dist <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a
