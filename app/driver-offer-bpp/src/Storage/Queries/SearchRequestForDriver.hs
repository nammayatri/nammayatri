module Storage.Queries.SearchRequestForDriver where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person
import Domain.Types.SearchRequest
import Domain.Types.SearchRequestForDriver
import Storage.Tabular.SearchRequestForDriver

create :: SearchRequestForDriver -> SqlDB ()
create = Esq.create

findByDriverAndSearchReq :: Transactionable m => Id Person -> Id SearchRequest -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchReq driverId searchReqId = Esq.findOne $ do
  sReq <- from $ table @SearchRequestForDriverT
  where_ $
    sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
      &&. sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
  pure sReq

findByDriver :: (Transactionable m, MonadTime m) => Id Person -> m [SearchRequestForDriver]
findByDriver driverId = do
  now <- getCurrentTime
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
        &&. sReq ^. SearchRequestForDriverSearchRequestValidTill >. val now
    orderBy [desc $ sReq ^. SearchRequestForDriverSearchRequestValidTill]
    pure sReq

removeAllBySearchId :: Id SearchRequest -> SqlDB ()
removeAllBySearchId searchReqId = Esq.delete $ do
  sReqForDriver <- from $ table @SearchRequestForDriverT
  where_ $ sReqForDriver ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
