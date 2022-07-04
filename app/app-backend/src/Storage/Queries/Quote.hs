{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.SearchRequest
import Storage.Queries.FullEntityBuilders (buildFullQuote)
import Storage.Tabular.Quote

create :: Quote -> SqlDB ()
create quote = do
  Esq.withFullEntity quote $ \(quoteT, quoteTermsT, quoteDetailsT) -> do
    Esq.create' quoteT
    traverse_ Esq.create' quoteTermsT
    case quoteDetailsT of
      OneWayDetailsT -> pure ()
      RentalDetailsT rentalQuoteT -> do
        Esq.create' rentalQuoteT

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId = Esq.buildDType $ do
  quoteT <- Esq.findById' quoteId
  join <$> mapM buildFullQuote quoteT

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId = Esq.buildDType $ do
  quoteT <- Esq.findAll' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    return quote
  catMaybes <$> mapM buildFullQuote quoteT
