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

create :: forall m. Monad m => SearchRequest -> SqlDB m ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' @SearchReqLocationT @m fromLoc
    Esq.create' toLoc
    Esq.create' sReq

findById :: forall m ma. Transactionable ma m => Id SearchRequest -> Proxy ma -> m (Maybe SearchRequest)
findById searchRequestId _ = buildDType $
  fmap (fmap $ extractSolidType @Domain.SearchRequest) $
    Esq.findOne' @m @ma $ do
      (sReq :& sFromLoc :& sToLoc) <-
        from
          ( table @SearchRequestT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestFromLocationId ==. loc1 ^. SearchReqLocationTId)
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchRequestToLocationId ==. loc2 ^. SearchReqLocationTId)
          )
      where_ $ sReq ^. SearchRequestTId ==. val (toKey searchRequestId)
      pure (sReq, sFromLoc, sToLoc)

updateStatus ::
  Id SearchRequest ->
  SearchRequestStatus ->
  SqlDB m ()
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
  forall m ma.
  Transactionable ma m =>
  Id SearchRequest ->
  Proxy ma ->
  m (Maybe (Id SearchRequest))
getRequestIdfromTransactionId tId _ = do
  findOne @m @ma $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTransactionId ==. val (getId tId)
    return $ searchT ^. SearchRequestTId

getStatus ::
  forall m ma.
  Transactionable ma m =>
  Id SearchRequest ->
  Proxy ma ->
  m (Maybe SearchRequestStatus)
getStatus searchRequestId _ = do
  findOne @m @ma $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchRequestStatus

getValidTill ::
  forall m ma.
  (Transactionable ma m) =>
  Id SearchRequest ->
  Proxy ma ->
  m (Maybe UTCTime)
getValidTill searchRequestId _ = do
  findOne @m @ma $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchRequestValidTill
