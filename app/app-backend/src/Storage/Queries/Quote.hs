{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Data.Tuple.Extra
import Domain.Types.Quote
import Domain.Types.SearchRequest
import Storage.Queries.FullEntityBuilders (buildFullQuote)
import Storage.Tabular.Quote
import Storage.Tabular.Quote.Instances

createDetails :: QuoteDetailsT -> FullEntitySqlDB ()
createDetails = \case
  OneWayDetailsT -> pure ()
  RentalDetailsT rentalSlabT -> do
    Esq.create' rentalSlabT
  AutoDetailsT -> pure ()

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
    Esq.createMany' quoteTs
    Esq.createMany' mbTripTermsTs
    traverse_ createDetails quoteDetailsTs

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId = Esq.buildDType $ do
  quoteT <- Esq.findById' quoteId
  join <$> mapM buildFullQuote quoteT

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId = Esq.buildDType $ do
  quoteT <- Esq.findAll' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    return quote
  catMaybes <$> mapM buildFullQuote quoteT
