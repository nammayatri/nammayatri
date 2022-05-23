module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Products
import Domain.Types.Quote
import Domain.Types.Quote.OneWayQuote
import Domain.Types.SearchRequest
import Storage.Tabular.Quote

create :: Quote -> SqlDB ()
create quote = do
  create' quote
  case quote.quoteDetails of
    OneWayDetails oneWayDetails -> create' (mkOneWayQuote quote.id oneWayDetails)
    RentalDetails _ -> pure ()

findAllByProductIds :: Transactionable m => Integer -> Integer -> [Id Products] -> m [Quote]
findAllByProductIds limit_ offset_ ids = do
  findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteProductId `in_` valList (toKey <$> ids)
    orderBy [desc $ quote ^. QuoteCreatedAt]
    limit $ fromIntegral limit_
    offset $ fromIntegral offset_
    return quote

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId requestId =
  findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey requestId)
    return quote

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById = Esq.findById
