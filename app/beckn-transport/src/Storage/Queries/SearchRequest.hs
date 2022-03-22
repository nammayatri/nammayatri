module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Organization
import Domain.Types.SearchRequest
import Storage.Tabular.SearchRequest

create :: SearchRequest -> SqlDB ()
create = Esq.create'

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById = Esq.findById

findByTxnIdAndBapIdAndBppId :: Transactionable m => Text -> Text -> Id Organization -> m (Maybe SearchRequest)
findByTxnIdAndBapIdAndBppId txnId bapId orgId =
  Esq.findOne $ do
    search <- from $ table @SearchRequestT
    where_ $
      search ^. SearchRequestTransactionId ==. val txnId
        &&. search ^. SearchRequestProviderId ==. val (toKey orgId)
        &&. search ^. SearchRequestBapId ==. val bapId
    return search

findByTxnIdAndProviderId :: Transactionable m => Text -> Id Organization -> m (Maybe SearchRequest)
findByTxnIdAndProviderId txnId orgId =
  Esq.findOne $ do
    search <- from $ table @SearchRequestT
    where_ $
      search ^. SearchRequestTransactionId ==. val txnId
        &&. search ^. SearchRequestProviderId ==. val (toKey orgId)
    return search
