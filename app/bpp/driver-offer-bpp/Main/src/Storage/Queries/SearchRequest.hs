module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.SearchRequest as Domain
import Storage.Tabular.SearchRequest
import Storage.Tabular.SearchRequest.SearchReqLocation

create :: SearchRequest -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = buildDType $
  fmap (fmap $ extractSolidType @Domain.SearchRequest) $
    Esq.findOne' $ do
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