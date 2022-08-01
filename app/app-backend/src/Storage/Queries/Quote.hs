{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq hiding (Table)
import Beckn.Types.Id
import Data.Tuple.Extra
import Domain.Types.Estimate
import Domain.Types.Quote
import Domain.Types.SearchRequest
import Storage.Queries.FullEntityBuilders (buildFullQuote)
import Storage.Tabular.DriverOffer
import Storage.Tabular.Quote
import Storage.Tabular.Quote.Instances
import Storage.Tabular.RentalSlab
import Storage.Tabular.TripTerms

createDetails :: QuoteDetailsT -> FullEntitySqlDB ()
createDetails = \case
  OneWayDetailsT -> pure ()
  RentalDetailsT rentalSlabT -> do
    Esq.create' rentalSlabT
  DriverOfferDetailsT driverOfferT -> do
    Esq.create' driverOfferT

-- order of creating entites make sense!
create :: Quote -> SqlDB ()
create quote =
  Esq.withFullEntity quote $ \(quoteT, mbTripTermsT, quoteDetailsT) -> do
    traverse_ Esq.create' mbTripTermsT
    createDetails quoteDetailsT
    Esq.create' quoteT

createMany :: [Quote] -> SqlDB ()
createMany quotes =
  Esq.withFullEntities quotes $ \list -> do
    let quoteTs = map fst3 list
        mbTripTermsTs = mapMaybe snd3 list
        quoteDetailsTs = map thd3 list
    Esq.createMany' mbTripTermsTs
    traverse_ createDetails quoteDetailsTs
    Esq.createMany' quoteTs

-- remove when https://bitbucket.org/juspay/beckn-shared-kernel/pull-requests/35 will be merged
type Table a = SqlExpr (Entity a)

type MbTable a = SqlExpr (Maybe (Entity a))

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

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId = Esq.buildDType $ do
  mbFullQuoteT <- Esq.findOne' $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer) <- from fullQuoteTable
    where_ $ quote ^. QuoteTId ==. val (toKey quoteId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)
  join <$> mapM buildFullQuote mbFullQuoteT

findByBppIdAndQuoteId :: Transactionable m => Text -> Id BPPQuote -> m (Maybe Quote)
findByBppIdAndQuoteId bppId quoteId = buildDType $ do
  mbFullQuoteT <- Esq.findOne' $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer) <- from fullQuoteTable
    where_ $
      quote ^. QuoteProviderId ==. val bppId
        &&. mbDriverOffer ?. DriverOfferBppQuoteId ==. just (val quoteId.getId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)
  join <$> mapM buildFullQuote mbFullQuoteT

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId = Esq.buildDType $ do
  fullQuoteTs <- Esq.findAll' $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer) <- from fullQuoteTable
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)
  catMaybes <$> mapM buildFullQuote fullQuoteTs

findAllByEstimateId :: Transactionable m => Id Estimate -> m [Quote]
findAllByEstimateId estimateId = buildDType $ do
  fullQuoteTs <- Esq.findAll' $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer) <- from fullQuoteTable
    where_ $
      mbDriverOffer ?. DriverOfferEstimateId ==. just (val $ toKey estimateId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)
  catMaybes <$> mapM buildFullQuote fullQuoteTs
