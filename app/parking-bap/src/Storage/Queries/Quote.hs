module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.ParkingLocation
import Domain.Quote
import Domain.Search
import Storage.Tabular.ParkingLocation
import Storage.Tabular.Quote

findById :: EsqDBFlow m r => Id Quote -> m (Maybe Quote)
findById = Esq.findById

create :: Quote -> SqlDB ()
create = create'

findAllBySearchId :: EsqDBFlow m r => Id Search -> m [Quote]
findAllBySearchId searchId =
  runTransaction . findAll' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return quote

findAllAggregatesBySearchId :: EsqDBFlow m r => Id Search -> m [(Quote, ParkingLocation)]
findAllAggregatesBySearchId searchId =
  runTransaction . findAll' $ do
    (quote :& location) <-
      from $
        table @QuoteT
          `innerJoin` table @ParkingLocationT
            `Esq.on` ( \(quote :& location) ->
                         quote ^. QuoteParkingLocationId ==. location ^. ParkingLocationTId
                     )
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return (quote, location)
