{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.Quote.QuoteTerms
import Domain.Types.Quote.RentalQuote
import Domain.Types.SearchRequest
import Storage.Tabular.Quote

create :: Quote -> SqlDB ()
create quote = do
  create' quote
  traverse_ create' (mkQuoteTermsEntities quote)
  case quote.quoteDetails of
    OneWayDetails _ -> pure ()
    RentalDetails rentalDetails -> create' (mkRentalQuote quote.id rentalDetails)

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById = Esq.findById

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId =
  Esq.findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    return quote
