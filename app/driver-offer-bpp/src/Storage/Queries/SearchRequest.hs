module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.SearchRequest
import Storage.Tabular.FareParameters
import Storage.Tabular.SearchRequest
import Storage.Tabular.SearchRequest.SearchReqLocation

create :: SearchRequest -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc, fareParams) -> do
    Esq.create' fareParams
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = buildDType $
  fmap (fmap extractSolidType) $
    Esq.findOne' $ do
      (sReq :& sFromLoc :& sToLoc :& fareParams) <-
        from
          ( table @SearchRequestT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestFromLocationId ==. loc1 ^. SearchReqLocationTId)
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchRequestToLocationId ==. loc2 ^. SearchReqLocationTId)
              `innerJoin` table @FareParametersT
                `Esq.on` ( \(s :& _ :& _ :& farePars) ->
                             s ^. SearchRequestFareParametersId ==. farePars ^. FareParametersTId
                         )
          )
      where_ $ sReq ^. SearchRequestTId ==. val (toKey searchRequestId)
      pure (sReq, sFromLoc, sToLoc, fareParams)
