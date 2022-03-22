module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.SearchRequest
import Storage.Tabular.Quote

create :: Quote -> SqlDB ()
create = create'

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById = Esq.findById

findByBPPQuoteId :: Transactionable m => Id BPPQuote -> m (Maybe Quote)
findByBPPQuoteId bppQuoteId_ =
  Esq.findOne $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteBppQuoteId ==. val (getId bppQuoteId_)
    return quote

findByTxnIdAndBppIdAndQuoteId :: Transactionable m => Id SearchRequest -> Text -> Id BPPQuote -> m (Maybe Quote)
findByTxnIdAndBppIdAndQuoteId txnId bppId quoteId =
  Esq.findOne $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey txnId) &&. quote ^. QuoteProviderId ==. val bppId &&. quote ^. QuoteBppQuoteId ==. val (getId quoteId)
    return quote

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId =
  Esq.findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    return quote
