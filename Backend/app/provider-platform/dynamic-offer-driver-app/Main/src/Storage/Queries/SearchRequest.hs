{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequest where

import Domain.Types.SearchRequest as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.SearchRequest
import Storage.Tabular.SearchRequest.SearchReqLocation

create :: SearchRequest -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

updateStatus ::
  Id SearchRequest ->
  SearchRequestStatus ->
  SqlDB ()
updateStatus searchId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchRequestUpdatedAt =. val now,
        SearchRequestStatus =. val status_
      ]
    where_ $ tbl ^. SearchRequestTId ==. val (toKey searchId)

getRequestIdfromTransactionId ::
  (Transactionable m) =>
  Id SearchRequest ->
  m (Maybe (Id SearchRequest))
getRequestIdfromTransactionId tId = do
  findOne $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTransactionId ==. val (getId tId)
    return $ searchT ^. SearchRequestTId

getStatus ::
  (Transactionable m) =>
  Id SearchRequest ->
  m (Maybe SearchRequestStatus)
getStatus searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchRequestStatus

getValidTill ::
  (Transactionable m) =>
  Id SearchRequest ->
  m (Maybe UTCTime)
getValidTill searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchRequestValidTill

-- queries fetching only one entity should avoid join for performance reason

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = Esq.buildDType . runMaybeT $ do
  searchRequest <- Esq.findByIdM @SearchRequestT $ toKey searchRequestId
  fetchFullSearchRequestM searchRequest

-- internal queries for building domain types

fetchFullSearchRequestM ::
  Transactionable m =>
  SearchRequestT ->
  MaybeT (DTypeBuilder m) (SolidType FullSearchRequestT)
fetchFullSearchRequestM searchRequest@SearchRequestT {..} = do
  fromLocation <- Esq.findByIdM @SearchReqLocationT fromLocationId
  toLocation <- Esq.findByIdM @SearchReqLocationT toLocationId
  pure $ extractSolidType @SearchRequest (searchRequest, fromLocation, toLocation)