module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.Search
import Domain.Types.TransportStation
import Storage.Tabular.Quote
import Storage.Tabular.TransportStation

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

findAllAggregatesBySearchId :: EsqDBFlow m r => Id Search -> m [(Quote, TransportStation, TransportStation)]
findAllAggregatesBySearchId searchId =
  runTransaction . findAll' $ do
    (quote :& depStation :& arrStation) <-
      from $
        table @QuoteT
          `innerJoin` table @TransportStationT
            `Esq.on` ( \(quote :& depStation) ->
                         quote ^. QuoteDepartureStationId ==. depStation ^. TransportStationTId
                     )
          `innerJoin` table @TransportStationT
            `Esq.on` ( \(quote :& _ :& arrStation) ->
                         quote ^. QuoteDepartureStationId ==. arrStation ^. TransportStationTId
                     )
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return (quote, depStation, arrStation)
