module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.SearchRequest
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Quote

create :: Quote -> SqlDB ()
create quote =
  Esq.withFullEntity quote $ \(quoteT, quoteDetails) -> do
    create' quoteT
    case quoteDetails of
      OneWayDetailsT oneWayQuoteT -> create' oneWayQuoteT
      RentalDetailsT (rentalQuoteT, _) -> create' rentalQuoteT

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId requestId = Esq.buildDType $ do
  quoteT <- Esq.findAll' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey requestId)
    return quote
  catMaybes <$> mapM buildFullQuote quoteT

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId = Esq.buildDType $ do
  quoteT <- Esq.findById' quoteId
  join <$> mapM buildFullQuote quoteT
