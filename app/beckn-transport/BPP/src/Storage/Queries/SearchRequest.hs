{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
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

findByMsgIdAndBapIdAndBppId :: Transactionable m => Text -> Text -> Id Organization -> m (Maybe SearchRequest)
findByMsgIdAndBapIdAndBppId txnId bapId orgId = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' $ do
    (sReq :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $
      sReq ^. SearchRequestMessageId ==. val txnId
        &&. sReq ^. SearchRequestProviderId ==. val (toKey orgId)
        &&. sReq ^. SearchRequestBapId ==. val bapId
    pure (sReq, sFromLoc, mbSToLoc)
  pure $ extractSolidType <$> mbFullSearchReqT
