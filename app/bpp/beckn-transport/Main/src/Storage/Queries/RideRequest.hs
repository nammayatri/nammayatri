module Storage.Queries.RideRequest where

import Domain.Types.Merchant
import Domain.Types.RideRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.RideRequest

create :: RideRequest -> SqlDB ()
create = Esq.create

fetchOldest :: Transactionable m => ShortId Subscriber -> Integer -> m [RideRequest]
fetchOldest subscriberId limit' = do
  let limitVal = fromIntegral limit'
  Esq.findAll $ do
    rideRequest <- from $ table @RideRequestT
    where_ $ rideRequest ^. RideRequestSubscriberId ==. val (getShortId subscriberId)
    orderBy [asc $ rideRequest ^. RideRequestCreatedAt]
    limit limitVal
    return rideRequest

removeRequest :: Id RideRequest -> SqlDB ()
removeRequest = Esq.deleteByKey @RideRequestT
