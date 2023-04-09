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

create :: SearchRequest -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, mbToLoc) -> do
    Esq.create' fromLoc
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

findAllByPerson :: Transactionable m => Id Person -> m [SearchRequest]
findAllByPerson perId = Esq.buildDType $ do
  fullSearchRequestsT <- Esq.findAll' $ do
    (searchRequest :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey perId)
    return (searchRequest, sFromLoc, mbSToLoc)
  pure $ extractSolidType @SearchRequest <$> fullSearchRequestsT

-- queries fetching only one entity should avoid join for performance reason

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = Esq.buildDType . runMaybeT $ do
  searchRequest <- Esq.findByIdM @SearchRequestT $ toKey searchRequestId
  fetchFullSearchRequestM searchRequest

findByPersonId :: Transactionable m => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId personId searchRequestId = Esq.buildDType . runMaybeT $ do
  searchRequest <- Esq.findOneM $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
        &&. searchRequest ^. SearchRequestId ==. val (getId searchRequestId)
    return searchRequest
  fetchFullSearchRequestM searchRequest

-- internal queries for building domain types

fetchFullSearchRequestM ::
  Transactionable m =>
  SearchRequestT ->
  MaybeT (DTypeBuilder m) (SolidType FullSearchRequestT)
fetchFullSearchRequestM searchRequest@SearchRequestT {..} = do
  fromLocation <- Esq.findByIdM @SearchReqLocationT fromLocationId
  mbToLocation <- forM toLocationId $ Esq.findByIdM @SearchReqLocationT
  pure $ extractSolidType @SearchRequest (searchRequest, fromLocation, mbToLocation)
