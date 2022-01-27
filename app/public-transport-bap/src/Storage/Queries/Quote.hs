module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.Search
import Storage.Tabular.Quote

findById :: EsqDBFlow m r => Id Quote -> m (Maybe Quote)
findById quoteId =
  runTransaction . findOne' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteId ==. val (getId quoteId)
    return quote

create :: Quote -> SqlDB ()
create = create'

findAllBySearchId :: EsqDBFlow m r => Id Search -> m [Quote]
findAllBySearchId searchId =
  runTransaction . findAll' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return quote
