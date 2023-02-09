module Storage.Queries.SearchRequestForDriver where

import Domain.Types.Person
import Domain.Types.SearchRequest
import Domain.Types.SearchRequestForDriver as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.SearchRequestForDriver

createMany :: [SearchRequestForDriver] -> SqlDB ()
createMany = Esq.createMany

findAllActiveByRequestId :: (Transactionable m, MonadTime m) => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveByRequestId searchReqId = do
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
    pure sReq

findAllActiveWithoutRespByRequestId :: (Transactionable m, MonadTime m) => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveWithoutRespByRequestId searchReqId = do
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
        &&. Esq.isNothing (sReq ^. SearchRequestForDriverResponse)
    pure sReq

findByDriverAndSearchReq :: Transactionable m => Id Person -> Id SearchRequest -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchReq driverId searchReqId = Esq.findOne $ do
  sReq <- from $ table @SearchRequestForDriverT
  where_ $
    sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
      &&. sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
      &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
  pure sReq

findByDriver :: (Transactionable m, MonadTime m) => Id Person -> m [SearchRequestForDriver]
findByDriver driverId = do
  now <- getCurrentTime
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
        &&. sReq ^. SearchRequestForDriverSearchRequestValidTill >. val now
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
    orderBy [desc $ sReq ^. SearchRequestForDriverSearchRequestValidTill]
    pure sReq

removeAllBySearchId :: Id SearchRequest -> SqlDB ()
removeAllBySearchId searchReqId = Esq.delete $ do
  sReqForDriver <- from $ table @SearchRequestForDriverT
  where_ $ sReqForDriver ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId personId = Esq.delete $ do
  sReqForDriver <- from $ table @SearchRequestForDriverT
  where_ $ sReqForDriver ^. SearchRequestForDriverDriverId ==. val (toKey personId)

setInactiveByRequestId :: Id SearchRequest -> SqlDB ()
setInactiveByRequestId searchReqId = Esq.update $ \p -> do
  set p [SearchRequestForDriverStatus =. val Domain.Inactive]
  where_ $ p ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)

updateDriverResponse :: Id SearchRequestForDriver -> SearchRequestForDriverResponse -> SqlDB ()
updateDriverResponse id response = Esq.update $ \p -> do
  set p [SearchRequestForDriverResponse =. val (Just response)]
  where_ $ p ^. SearchRequestForDriverId ==. val (getId id)
