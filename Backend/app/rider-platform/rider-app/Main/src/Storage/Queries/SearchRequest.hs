{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SearchRequest where

import Domain.Types.LocationMapping as DLocationMapping
import Domain.Types.Merchant.MerchantPaymentMethod (MerchantPaymentMethod)
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders
import Storage.Queries.LocationMapping as QLocationMapping
import Storage.Tabular.SearchRequest

create :: SearchRequest -> SqlDB ()
create dsReq = do
  mappings <- locationMappingMaker dsReq DLocationMapping.SearchRequest
  Esq.runTransaction $
    withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
      Esq.create' sReq
      void $ Esq.createUnique' fromLoc
      traverse_ Esq.createUnique' toLoc
  QLocationMapping.createMany mappings

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = Esq.buildDType $ do
  res <- Esq.findOne' $ do
    sReq <- from $ table @SearchRequestT
    where_ $ sReq ^. SearchRequestTId ==. val (toKey searchRequestId)
    pure sReq
  join <$> mapM buildFullSearchRequest res

findByPersonId :: Transactionable m => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId personId searchRequestId = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
        &&. searchRequest ^. SearchRequestId ==. val (getId searchRequestId)
    return searchRequest
  join <$> mapM buildFullSearchRequest mbFullSearchReqT

findAllByPerson :: Transactionable m => Id Person -> m [SearchRequest]
findAllByPerson perId = Esq.buildDType $ do
  fullSearchRequestsT <- Esq.findAll' $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey perId)
    return searchRequest
  catMaybes <$> mapM buildFullSearchRequest fullSearchRequestsT

updateCustomerExtraFeeAndPaymentMethod :: Id SearchRequest -> Maybe Money -> Maybe (Id DMPM.MerchantPaymentMethod) -> SqlDB ()
updateCustomerExtraFeeAndPaymentMethod searchReqId customerExtraFee paymentMethodId =
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchRequestCustomerExtraFee =. val customerExtraFee,
        SearchRequestSelectedPaymentMethodId =. val (toKey <$> paymentMethodId)
      ]
    where_ $ tbl ^. SearchRequestId ==. val (getId searchReqId)

updateAutoAssign ::
  Id SearchRequest ->
  Bool ->
  Bool ->
  SqlDB ()
updateAutoAssign searchRequestId autoAssignedEnabled autoAssignedEnabledV2 =
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchRequestAutoAssignEnabled =. val (Just autoAssignedEnabled),
        SearchRequestAutoAssignEnabledV2 =. val (Just autoAssignedEnabledV2)
      ]
    where_ $ tbl ^. SearchRequestTId ==. val (toKey searchRequestId)

updatePaymentMethods :: Id SearchRequest -> [Id MerchantPaymentMethod] -> SqlDB ()
updatePaymentMethods searchReqId availablePaymentMethods =
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchRequestAvailablePaymentMethods =. val (PostgresList $ toKey <$> availablePaymentMethods)
      ]
    where_ $ tbl ^. SearchRequestId ==. val (getId searchReqId)
