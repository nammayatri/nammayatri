{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Vehicle.Internal where

import qualified Domain.Types.Person as DP
import Domain.Types.Vehicle as DV
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Vehicle as Vehicle

getVehicles ::
  Transactionable m =>
  [Id DP.Person] ->
  m [Vehicle]
getVehicles personIds = do
  Esq.findAll $ do
    vehicles <- from $ table @VehicleT
    where_ $
      vehicles ^. VehicleDriverId `in_` valList (toKey <$> personIds)
    return vehicles
