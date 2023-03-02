{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.RideRequest where

import Domain.Types.Merchant
import Domain.Types.RideRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.RideRequest

create :: RideRequest -> SqlDB m ()
create = Esq.create

fetchOldest :: forall m ma. Transactionable ma m => ShortId Subscriber -> Integer -> Proxy ma -> m [RideRequest]
fetchOldest subscriberId limit' _ = do
  let limitVal = fromIntegral limit'
  Esq.findAll @m @ma $ do
    rideRequest <- from $ table @RideRequestT
    where_ $ rideRequest ^. RideRequestSubscriberId ==. val (getShortId subscriberId)
    orderBy [asc $ rideRequest ^. RideRequestCreatedAt]
    limit limitVal
    return rideRequest

removeRequest :: Id RideRequest -> SqlDB m ()
removeRequest = Esq.deleteByKey @RideRequestT
