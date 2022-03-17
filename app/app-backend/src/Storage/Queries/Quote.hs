module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.SearchRequest
import Storage.Tabular.Quote

create :: Quote -> SqlDB ()
create = create'

findById :: EsqDBFlow m r => Id Quote -> m (Maybe Quote)
findById = Esq.findById

findByBPPQuoteId :: EsqDBFlow m r => Id BPPQuote -> m (Maybe Quote)
findByBPPQuoteId bppQuoteId_ =
  runTransaction . findOne' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteBppQuoteId ==. val (getId bppQuoteId_)
    return quote

findByTxnIdAndBppIdAndQuoteId :: EsqDBFlow m r => Id SearchRequest -> Text -> Id BPPQuote -> m (Maybe Quote)
findByTxnIdAndBppIdAndQuoteId txnId bppId quoteId =
  runTransaction . findOne' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey txnId) &&. quote ^. QuoteProviderId ==. val bppId &&. quote ^. QuoteBppQuoteId ==. val (getId quoteId)
    return quote

findAllByRequestId :: EsqDBFlow m r => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId =
  runTransaction . findAll' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    return quote
