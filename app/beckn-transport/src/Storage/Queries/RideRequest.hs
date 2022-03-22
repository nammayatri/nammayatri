module Storage.Queries.RideRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.RideRequest
import Storage.Tabular.RideRequest

create :: RideRequest -> SqlDB ()
create = Esq.create'

fetchOldest :: Transactionable m => ShortId Organization -> Integer -> m [RideRequest]
fetchOldest shortId limit' = do
  let limitVal = fromIntegral limit'
  Esq.findAll $ do
    rideRequest <- from $ table @RideRequestT
    where_ $ rideRequest ^. RideRequestShortOrgId ==. val (getShortId shortId)
    orderBy [asc $ rideRequest ^. RideRequestCreatedAt]
    limit limitVal
    return rideRequest

removeRequest :: Id RideRequest -> SqlDB ()
removeRequest = Esq.deleteByKey' @RideRequestT
