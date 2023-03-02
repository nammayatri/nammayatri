{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Data.Tuple.Extra
import Domain.Types.DriverOffer
import Domain.Types.Estimate
import Domain.Types.Quote
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders (buildFullQuote)
import Storage.Tabular.DriverOffer
import Storage.Tabular.Quote
import Storage.Tabular.Quote.Instances
import Storage.Tabular.RentalSlab
import Storage.Tabular.TripTerms

createDetails :: QuoteDetailsT -> FullEntitySqlDB m ()
createDetails = \case
  OneWayDetailsT -> pure ()
  RentalDetailsT rentalSlabT -> do
    Esq.create' rentalSlabT
  DriverOfferDetailsT driverOfferT -> do
    Esq.create' driverOfferT

-- order of creating entites make sense!
create :: Quote -> SqlDB m ()
create quote =
  Esq.withFullEntity quote $ \(quoteT, mbTripTermsT, quoteDetailsT) -> do
    traverse_ Esq.create' mbTripTermsT
    createDetails quoteDetailsT
    Esq.create' quoteT

createMany :: [Quote] -> SqlDB m ()
createMany quotes =
  Esq.withFullEntities quotes $ \list -> do
    let quoteTs = map fst3 list
        mbTripTermsTs = mapMaybe snd3 list
        quoteDetailsTs = map thd3 list
    Esq.createMany' mbTripTermsTs
    traverse_ createDetails quoteDetailsTs
    Esq.createMany' quoteTs

fullQuoteTable ::
  From
    ( Table QuoteT
        :& MbTable TripTermsT
        :& MbTable RentalSlabT
        :& MbTable DriverOfferT
    )
fullQuoteTable =
  table @QuoteT
    `leftJoin` table @TripTermsT
      `Esq.on` ( \(quote :& mbTripTerms) ->
                   quote ^. QuoteTripTermsId ==. mbTripTerms ?. TripTermsTId
               )
    `leftJoin` table @RentalSlabT
      `Esq.on` ( \(quote :& _ :& mbRentalSlab) ->
                   quote ^. QuoteRentalSlabId ==. mbRentalSlab ?. RentalSlabTId
               )
    `leftJoin` table @DriverOfferT
      `Esq.on` ( \(quote :& _ :& _ :& mbDriverOffer) ->
                   quote ^. QuoteDriverOfferId ==. mbDriverOffer ?. DriverOfferTId
               )

findById :: forall m ma. Transactionable ma m => Id Quote -> Proxy ma -> m (Maybe Quote)
findById quoteId proxy = Esq.buildDType $ do
  mbFullQuoteT <- Esq.findOne' @m @ma $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer) <- from fullQuoteTable
    where_ $ quote ^. QuoteTId ==. val (toKey quoteId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)
  join <$> mapM (`buildFullQuote` proxy) mbFullQuoteT

findByBppIdAndBPPQuoteId :: forall m ma. Transactionable ma m => Text -> Id BPPQuote -> Proxy ma -> m (Maybe Quote)
findByBppIdAndBPPQuoteId bppId bppQuoteId proxy = buildDType $ do
  mbFullQuoteT <- Esq.findOne' @m @ma $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer) <- from fullQuoteTable
    where_ $
      quote ^. QuoteProviderId ==. val bppId
        &&. mbDriverOffer ?. DriverOfferBppQuoteId ==. just (val bppQuoteId.getId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)
  join <$> mapM (`buildFullQuote` proxy) mbFullQuoteT

findAllByRequestId :: forall m ma. Transactionable ma m => Id SearchRequest -> Proxy ma -> m [Quote]
findAllByRequestId searchRequestId proxy = Esq.buildDType $ do
  fullQuoteTs <- Esq.findAll' @m @ma $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer) <- from fullQuoteTable
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)
  catMaybes <$> mapM (`buildFullQuote` proxy) fullQuoteTs

findAllByEstimateId :: forall m ma. Transactionable ma m => Id Estimate -> Proxy ma -> m [Quote]
findAllByEstimateId estimateId proxy = buildDType $ do
  fullQuoteTs <- Esq.findAll' @m @ma $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer) <- from fullQuoteTable
    where_ $
      mbDriverOffer ?. DriverOfferEstimateId ==. just (val $ toKey estimateId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)
  catMaybes <$> mapM (`buildFullQuote` proxy) fullQuoteTs
