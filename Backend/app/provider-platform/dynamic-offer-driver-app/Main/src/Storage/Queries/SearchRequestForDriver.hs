{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequestForDriver where

import Domain.Types.Person
import Domain.Types.SearchRequest
import Domain.Types.SearchRequestForDriver as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.SearchRequestForDriver

createMany :: [SearchRequestForDriver] -> SqlDB m ()
createMany = Esq.createMany

findAllActiveByRequestId :: forall m ma. (Transactionable ma m, MonadTime m) => Id SearchRequest -> Proxy ma -> m [SearchRequestForDriver]
findAllActiveByRequestId searchReqId _ = do
  Esq.findAll @m @ma $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
    pure sReq

findAllActiveWithoutRespByRequestId :: forall m ma. (Transactionable ma m, MonadTime m) => Id SearchRequest -> Proxy ma -> m [SearchRequestForDriver]
findAllActiveWithoutRespByRequestId searchReqId _ = do
  Esq.findAll @m @ma $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
        &&. Esq.isNothing (sReq ^. SearchRequestForDriverResponse)
    pure sReq

findByDriverAndSearchReq :: forall m ma. Transactionable ma m => Id Person -> Id SearchRequest -> Proxy ma -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchReq driverId searchReqId _ = Esq.findOne @m @ma $ do
  sReq <- from $ table @SearchRequestForDriverT
  where_ $
    sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
      &&. sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
      &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
  pure sReq

findByDriver :: forall m ma. (Transactionable ma m, MonadTime m) => Id Person -> Proxy ma -> m [SearchRequestForDriver]
findByDriver driverId _ = do
  now <- getCurrentTime
  Esq.findAll @m @ma $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
        &&. sReq ^. SearchRequestForDriverSearchRequestValidTill >. val now
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
    orderBy [desc $ sReq ^. SearchRequestForDriverSearchRequestValidTill]
    pure sReq

removeAllBySearchId :: Id SearchRequest -> SqlDB m ()
removeAllBySearchId searchReqId = Esq.delete $ do
  sReqForDriver <- from $ table @SearchRequestForDriverT
  where_ $ sReqForDriver ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)

deleteByDriverId :: Id Person -> SqlDB m ()
deleteByDriverId personId = Esq.delete $ do
  sReqForDriver <- from $ table @SearchRequestForDriverT
  where_ $ sReqForDriver ^. SearchRequestForDriverDriverId ==. val (toKey personId)

setInactiveByRequestId :: Id SearchRequest -> SqlDB m ()
setInactiveByRequestId searchReqId = Esq.update $ \p -> do
  set p [SearchRequestForDriverStatus =. val Domain.Inactive]
  where_ $ p ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)

updateDriverResponse :: Id SearchRequestForDriver -> SearchRequestForDriverResponse -> SqlDB m ()
updateDriverResponse id response = Esq.update $ \p -> do
  set p [SearchRequestForDriverResponse =. val (Just response)]
  where_ $ p ^. SearchRequestForDriverId ==. val (getId id)
