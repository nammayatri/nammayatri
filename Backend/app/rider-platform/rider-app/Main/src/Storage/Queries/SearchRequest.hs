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

import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.SearchRequest
import Storage.Tabular.SearchRequest.SearchReqLocation

create :: forall m. Monad m => SearchRequest -> SqlDB m ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, mbToLoc) -> do
    Esq.create' @SearchReqLocationT @m fromLoc
    traverse_ Esq.create' mbToLoc
    Esq.create' sReq

fullSearchRequestTable ::
  From
    ( Table SearchRequestT
        :& Table SearchReqLocationT
        :& MbTable SearchReqLocationT
    )
fullSearchRequestTable =
  table @SearchRequestT
    `innerJoin` table @SearchReqLocationT
      `Esq.on` ( \(s :& loc1) ->
                   s ^. SearchRequestFromLocationId ==. loc1 ^. SearchReqLocationTId
               )
    `leftJoin` table @SearchReqLocationT
      `Esq.on` ( \(s :& _ :& mbLoc2) ->
                   s ^. SearchRequestToLocationId ==. mbLoc2 ?. SearchReqLocationTId
               )

findById :: forall m ma. Transactionable ma m => Id SearchRequest -> Proxy ma -> m (Maybe SearchRequest)
findById searchRequestId _ = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' @m @ma $ do
    (sReq :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $ sReq ^. SearchRequestTId ==. val (toKey searchRequestId)
    pure (sReq, sFromLoc, mbSToLoc)
  pure $ extractSolidType @SearchRequest <$> mbFullSearchReqT

findByPersonId :: forall m ma. Transactionable ma m => Id Person -> Id SearchRequest -> Proxy ma -> m (Maybe SearchRequest)
findByPersonId personId searchRequestId _ = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' @m @ma $ do
    (searchRequest :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
        &&. searchRequest ^. SearchRequestId ==. val (getId searchRequestId)
    return (searchRequest, sFromLoc, mbSToLoc)
  pure $ extractSolidType @SearchRequest <$> mbFullSearchReqT

findAllByPerson :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m [SearchRequest]
findAllByPerson perId _ = Esq.buildDType $ do
  fullSearchRequestsT <- Esq.findAll' @m @ma $ do
    (searchRequest :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey perId)
    return (searchRequest, sFromLoc, mbSToLoc)
  pure $ extractSolidType @SearchRequest <$> fullSearchRequestsT
