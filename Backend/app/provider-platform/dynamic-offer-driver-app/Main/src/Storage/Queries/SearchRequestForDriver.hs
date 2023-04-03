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
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver as Domain
import Domain.Types.SearchStep
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.SearchRequestForDriver

createMany :: [SearchRequestForDriver] -> SqlDB ()
createMany = Esq.createMany

findAllActiveBySRId :: (Transactionable m, MonadTime m) => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveBySRId searchReqId = do
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverRequestId ==. val (toKey searchReqId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
    pure sReq

findAllActiveBySSId :: (Transactionable m, MonadTime m) => Id SearchStep -> m [SearchRequestForDriver]
findAllActiveBySSId searchStepId = do
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchStepId ==. val (toKey searchStepId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
    pure sReq

findAllActiveWithoutRespBySearchStepId :: (Transactionable m, MonadTime m) => Id SearchStep -> m [SearchRequestForDriver]
findAllActiveWithoutRespBySearchStepId searchStepId = do
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchStepId ==. val (toKey searchStepId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
        &&. Esq.isNothing (sReq ^. SearchRequestForDriverResponse)
    pure sReq

findByDriverAndSearchStepId :: Transactionable m => Id Person -> Id SearchStep -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchStepId driverId searchStepId = Esq.findOne $ do
  sReq <- from $ table @SearchRequestForDriverT
  where_ $
    sReq ^. SearchRequestForDriverSearchStepId ==. val (toKey searchStepId)
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

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId personId = Esq.delete $ do
  sReqForDriver <- from $ table @SearchRequestForDriverT
  where_ $ sReqForDriver ^. SearchRequestForDriverDriverId ==. val (toKey personId)

setInactiveBySSId :: Id SearchStep -> SqlDB ()
setInactiveBySSId searchStepId = Esq.update $ \p -> do
  set p [SearchRequestForDriverStatus =. val Domain.Inactive]
  where_ $ p ^. SearchRequestForDriverSearchStepId ==. val (toKey searchStepId)

setInactiveBySRId :: Id SearchRequest -> SqlDB ()
setInactiveBySRId searchRequestId = Esq.update $ \p -> do
  set p [SearchRequestForDriverStatus =. val Domain.Inactive]
  where_ $ p ^. SearchRequestForDriverRequestId ==. val (toKey searchRequestId)

updateDriverResponse :: Id SearchRequestForDriver -> SearchRequestForDriverResponse -> SqlDB ()
updateDriverResponse id response = Esq.update $ \p -> do
  set p [SearchRequestForDriverResponse =. val (Just response)]
  where_ $ p ^. SearchRequestForDriverId ==. val (getId id)
