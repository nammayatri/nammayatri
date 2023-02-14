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

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId =
  Esq.findOne $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteId ==. val (getId quoteId)
    return quote

create :: Quote -> SqlDB ()
create = Esq.create

findAllBySearchId :: Transactionable m => Id Search -> m [Quote]
findAllBySearchId searchId =
  Esq.findAll $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return quote

findAllAggregatesBySearchId :: Transactionable m => Id Search -> m [(Quote, TransportStation, TransportStation)]
findAllAggregatesBySearchId searchId =
  findAll $ do
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
