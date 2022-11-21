{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Data.Tuple.Extra
import Domain.Types.DriverOffer
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

--whereCondition =
fullQuoteTable' ::
  Transactionable m =>
  (Table QuoteT -> SqlExpr (Value Bool)) ->
  Maybe (SqlExpr (Maybe (Entity DriverOfferT)) -> SqlExpr (Value Bool)) ->
  DTypeBuilder
    m
    ( Maybe
        ( QuoteT,
          Maybe TripTermsT,
          Maybe RentalSlabT,
          Maybe DriverOfferT
        )
    )
fullQuoteTable' whereQuote whereDriverOffer = do
  runMaybeT $
    do
      quote <-
        MaybeT $
          Esq.findOne' $ do
            quote' <- from (table @QuoteT)
            where_ $ whereQuote quote'
            pure quote'
      mbTripTerms <-
        MaybeT $
          Esq.findOne' $ do
            mbTripTerms' <- toMaybe <$> from (table @TripTermsT)
            where_ $ mbTripTerms' ?. TripTermsTId ==. val (tripTermsId quote)
            pure mbTripTerms'
      mbRentalSlab <-
        MaybeT $
          Esq.findOne' $ do
            mbRentalSlab' <- toMaybe <$> from (table @RentalSlabT)
            where_ $ val (rentalSlabId quote) ==. mbRentalSlab' ?. RentalSlabTId
            pure mbRentalSlab'
      mbDriverOffer <-
        MaybeT $
          Esq.findOne' $ do
            mbDriverOffer' <- toMaybe <$> from (table @DriverOfferT)
            where_ $
              val (driverOfferId quote) ==. mbDriverOffer' ?. DriverOfferTId
                &&. maybe (val True) (\fn -> fn mbDriverOffer') whereDriverOffer
            pure mbDriverOffer'
      pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer)

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId = Esq.buildDType $ do
  mbFullQuoteT <-
    fullQuoteTable'
      (\quote -> quote ^. QuoteTId ==. val (toKey quoteId))
      Nothing
  join <$> mapM buildFullQuote mbFullQuoteT

findByBppIdAndBPPQuoteId :: Transactionable m => Text -> Id BPPQuote -> m (Maybe Quote)
findByBppIdAndBPPQuoteId bppId bppQuoteId = buildDType $ do
  mbFullQuoteT <-
    fullQuoteTable'
      (\quote -> quote ^. QuoteProviderId ==. val bppId)
      (Just (\mbDriverOffer -> mbDriverOffer ?. DriverOfferBppQuoteId ==. just (val bppQuoteId.getId)))
  join <$> mapM buildFullQuote mbFullQuoteT

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId = Esq.buildDType $ do
  quotes <- Esq.findAll' $ do
    quote <- from (table @QuoteT)
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    pure quote
  mbTripTerms <-
    mapM
      ( \quote ->
          Esq.findOne' $ do
            mbTripTerms' <- from (table @TripTermsT)
            where_ $ just (mbTripTerms' ^. TripTermsTId) ==. val (tripTermsId quote)
            pure mbTripTerms'
      )
      quotes
  mbRentalSlabs <-
    mapM
      ( \quote ->
          Esq.findOne' $ do
            mbRentalSlab' <- from (table @RentalSlabT)
            where_ $ val (rentalSlabId quote) ==. just (mbRentalSlab' ^. RentalSlabTId)
            pure mbRentalSlab'
      )
      quotes
  mbDriverOffers <-
    mapM
      ( \quote ->
          Esq.findOne' $ do
            mbDriverOffer' <- from (table @DriverOfferT)
            where_ $ val (driverOfferId quote) ==. just (mbDriverOffer' ^. DriverOfferTId)
            pure mbDriverOffer'
      )
      quotes
  let fullQuoteTs = zip4 quotes mbTripTerms mbRentalSlabs mbDriverOffers
  catMaybes <$> mapM buildFullQuote fullQuoteTs
  where
    zip4 (x : xs) (y : yx) (z : zx) (k : kx) = (x, y, z, k) : zip4 xs yx zx kx
    zip4 _ _ _ _ = []

findAllByEstimateId :: Transactionable m => Id Estimate -> m [Quote]
findAllByEstimateId estimateId = buildDType $ do
  quotes <- Esq.findAll' $ from (table @QuoteT)
  mbTripTerms <-
    mapM
      ( \quote ->
          Esq.findOne' $ do
            mbTripTerms' <- from (table @TripTermsT)
            where_ $ just (mbTripTerms' ^. TripTermsTId) ==. val (tripTermsId quote)
            pure mbTripTerms'
      )
      quotes
  mbRentalSlabs <-
    mapM
      ( \quote ->
          Esq.findOne' $ do
            mbRentalSlab' <- from (table @RentalSlabT)
            where_ $ val (rentalSlabId quote) ==. just (mbRentalSlab' ^. RentalSlabTId)
            pure mbRentalSlab'
      )
      quotes
  mbDriverOffers <-
    mapM
      ( \quote ->
          Esq.findOne' $ do
            mbDriverOffer' <- from (table @DriverOfferT)
            where_ $
              val (driverOfferId quote) ==. just (mbDriverOffer' ^. DriverOfferTId)
                &&. mbDriverOffer' ^. DriverOfferEstimateId ==. val (toKey estimateId)
            pure mbDriverOffer'
      )
      quotes

  let fullQuoteTs = zip4 quotes mbTripTerms mbRentalSlabs mbDriverOffers
  catMaybes <$> mapM buildFullQuote fullQuoteTs
  where
    zip4 (x : xs) (y : yx) (z : zx) (k : kx) = (x, y, z, k) : zip4 xs yx zx kx
    zip4 _ _ _ _ = []
