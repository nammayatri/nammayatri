module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.ParkingLocation
import Domain.Quote
import Domain.Search
import Storage.Tabular.ParkingLocation
import Storage.Tabular.Quote

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById = Esq.findById

create :: Quote -> SqlDB ()
create = create'

findAllBySearchId :: Transactionable m => Id Search -> m [Quote]
findAllBySearchId searchId =
  Esq.findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return quote

findAllAggregatesBySearchId :: Transactionable m => Id Search -> m [(Quote, ParkingLocation)]
findAllAggregatesBySearchId searchId =
  findAll $ do
    (quote :& location) <-
      from $
        table @QuoteT
          `innerJoin` table @ParkingLocationT
            `Esq.on` ( \(quote :& location) ->
                         quote ^. QuoteParkingLocationId ==. location ^. ParkingLocationTId
                     )
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return (quote, location)
