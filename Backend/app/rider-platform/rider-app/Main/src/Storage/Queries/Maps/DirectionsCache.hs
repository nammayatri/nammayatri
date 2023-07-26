{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Maps.DirectionsCache
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Maps.DirectionsCache
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Maps.DirectionsCache

create :: DirectionsCache -> SqlDB ()
create = Esq.create

findRoute :: Transactionable m => Text -> Text -> Int -> m (Maybe DirectionsCache)
findRoute originHash destHash slot =
  Esq.findOne $ do
    directionsCache <- from $ table @DirectionsCacheT
    where_ $ directionsCache ^. DirectionsCacheOriginHash ==. val originHash &&. directionsCache ^. DirectionsCacheDestHash ==. val destHash &&. directionsCache ^. DirectionsCacheSlot ==. val slot
    return directionsCache
