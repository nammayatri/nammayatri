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
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Quote

create :: Quote -> SqlDB m ()
create quote =
  Esq.withFullEntity quote $ \(quoteT, quoteDetails) -> do
    create' quoteT
    case quoteDetails of
      OneWayDetailsT oneWayQuoteT -> create' oneWayQuoteT
      RentalDetailsT (rentalQuoteT, _) -> create' rentalQuoteT

findAllByRequestId :: forall m ma. Transactionable ma m => Id SearchRequest -> Proxy ma -> m [Quote]
findAllByRequestId requestId proxy = Esq.buildDType $ do
  quoteT <- Esq.findAll' @m @ma $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey requestId)
    return quote
  catMaybes <$> mapM (`buildFullQuote` proxy) quoteT

findById :: forall m ma. Transactionable ma m => Id Quote -> Proxy ma -> m (Maybe Quote)
findById quoteId proxy = Esq.buildDType $ do
  quoteT <- Esq.findById' @_ @m @ma quoteId
  join <$> mapM (`buildFullQuote` proxy) quoteT
