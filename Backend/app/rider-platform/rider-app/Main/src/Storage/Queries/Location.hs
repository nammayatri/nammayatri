{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Location where

import Domain.Types.Location
import Domain.Types.LocationAddress
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Location

findById :: Transactionable m => Id Location -> m (Maybe Location)
findById = Esq.findById

create :: Location -> SqlDB ()
create = Esq.create

updateLocation :: Id Location -> Location -> SqlDB ()
updateLocation blId Location {..} = do
  let LocationAddress {..} = address
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ LocationStreet =. val street,
        LocationCity =. val city,
        LocationState =. val state,
        LocationCountry =. val country,
        LocationBuilding =. val building,
        LocationAreaCode =. val areaCode,
        LocationArea =. val area,
        LocationUpdatedAt =. val now,
        LocationLat =. val lat,
        LocationLon =. val lon
      ]
    where_ $ tbl ^. LocationTId ==. val (toKey blId)
