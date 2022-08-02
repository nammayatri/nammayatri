{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
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

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' $ do
    (sReq :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $ sReq ^. SearchRequestTId ==. val (toKey searchRequestId)
    pure (sReq, sFromLoc, mbSToLoc)
  pure $ extractSolidType <$> mbFullSearchReqT

findByIdAndMerchantId ::
  Transactionable m =>
  Id SearchRequest ->
  Id Merchant ->
  m (Maybe SearchRequest)
findByIdAndMerchantId reqId merchantId = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' $ do
    (searchRequest :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $
      searchRequest ^. SearchRequestId ==. val (getId reqId)
        &&. searchRequest ^. SearchRequestMerchantId ==. val (toKey merchantId)
    return (searchRequest, sFromLoc, mbSToLoc)
  pure $ extractSolidType <$> mbFullSearchReqT

findByPersonId :: Transactionable m => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId personId searchRequestId = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' $ do
    (searchRequest :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
        &&. searchRequest ^. SearchRequestId ==. val (getId searchRequestId)
    return (searchRequest, sFromLoc, mbSToLoc)
  pure $ extractSolidType <$> mbFullSearchReqT

findAllByPerson :: Transactionable m => Id Person -> m [SearchRequest]
findAllByPerson perId = Esq.buildDType $ do
  fullSearchRequestsT <- Esq.findAll' $ do
    (searchRequest :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey perId)
    return (searchRequest, sFromLoc, mbSToLoc)
  pure $ extractSolidType <$> fullSearchRequestsT
