{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Location.SpecialLocation where

import qualified Domain.Types.Location.SpecialLocation as D
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Location.SpecialLocation

create :: D.SpecialLocation -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id D.SpecialLocation -> m (Maybe D.SpecialLocation)
findById = Esq.findById

findSpecialLocationByLatLong :: Transactionable m => LatLong -> m [D.SpecialLocation]
findSpecialLocationByLatLong point = do
  Esq.findAll $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $ containsPoint (point.lon, point.lat)
    return specialLocation
