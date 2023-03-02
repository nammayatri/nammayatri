{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Quote where

import Domain.Types.Quote
import Domain.Types.Search
import Domain.Types.TransportStation
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Quote
import Storage.Tabular.TransportStation

findById :: forall m ma. Transactionable ma m => Id Quote -> Proxy ma -> m (Maybe Quote)
findById quoteId _ =
  Esq.findOne @m @ma $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteId ==. val (getId quoteId)
    return quote

create :: Quote -> SqlDB m ()
create = Esq.create

findAllBySearchId :: forall m ma. Transactionable ma m => Id Search -> Proxy ma -> m [Quote]
findAllBySearchId searchId _ =
  Esq.findAll @m @ma $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return quote

findAllAggregatesBySearchId :: forall m ma. Transactionable ma m => Id Search -> Proxy ma -> m [(Quote, TransportStation, TransportStation)]
findAllAggregatesBySearchId searchId _ =
  findAll @m @ma $ do
    (quote :& depStation :& arrStation) <-
      from $
        table @QuoteT
          `innerJoin` table @TransportStationT
            `Esq.on` ( \(quote :& depStation) ->
                         quote ^. QuoteDepartureStationId ==. depStation ^. TransportStationTId
                     )
          `innerJoin` table @TransportStationT
            `Esq.on` ( \(quote :& _ :& arrStation) ->
                         quote ^. QuoteArrivalStationId ==. arrStation ^. TransportStationTId
                     )
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return (quote, depStation, arrStation)
