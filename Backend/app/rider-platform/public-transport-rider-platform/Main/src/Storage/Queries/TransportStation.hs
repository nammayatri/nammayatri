{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.TransportStation where

import Domain.Types.TransportStation
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.TransportStation

findByStationCode :: forall m ma. Transactionable ma m => Text -> Proxy ma -> m (Maybe TransportStation)
findByStationCode stationCode _ =
  Esq.findOne @m @ma $ do
    parkingLocation <- from $ table @TransportStationT
    where_ $ parkingLocation ^. TransportStationStationCode ==. val stationCode
    return parkingLocation

create :: TransportStation -> SqlDB m ()
create = Esq.create

findAll :: forall m ma. Transactionable ma m => Proxy ma -> m [TransportStation]
findAll _ =
  Esq.findAll @m @ma $ do
    from $ table @TransportStationT

findById :: forall m ma. Transactionable ma m => Id TransportStation -> Proxy ma -> m (Maybe TransportStation)
findById transportStationId _ =
  Esq.findOne @m @ma $ do
    transportStation <- from $ table @TransportStationT
    where_ $ transportStation ^. TransportStationTId ==. val (TransportStationTKey $ getId transportStationId)
    return transportStation
