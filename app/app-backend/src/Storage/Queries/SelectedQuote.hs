module Storage.Queries.SelectedQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id (Id)
import qualified Domain.Types.Quote as DQuote
import Domain.Types.SelectedQuote (SelectedQuote)
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.SelectedQuote
import Types.Common

-- order of creating entites make sense!
create :: SelectedQuote -> SqlDB ()
create quote =
  Esq.withFullEntity quote $ \(quoteT, mbTripTermsT) -> do
    traverse_ Esq.create' mbTripTermsT
    Esq.create' quoteT

createMany :: [SelectedQuote] -> SqlDB ()
createMany quotes =
  Esq.withFullEntities quotes $ \list -> do
    let quotesT = map fst list
        mbTripTermsTs = mapMaybe snd list
    Esq.createMany' mbTripTermsTs
    Esq.createMany' quotesT

findByBppIdAndQuoteId :: Transactionable m => Text -> Id BPPQuote -> m (Maybe SelectedQuote)
findByBppIdAndQuoteId bppId quoteId = buildDType $
  runMaybeT $ do
    quoteT <- MaybeT $
      Esq.findOne' $ do
        quote <- from $ table @SelectedQuoteT
        where_ $
          quote ^. SelectedQuoteProviderId ==. val bppId
            &&. quote ^. SelectedQuoteBppQuoteId ==. val quoteId.getId
        return quote
    MaybeT $ buildFullSelectedQuote quoteT

findByQuoteId :: Transactionable m => Id DQuote.Quote -> m [SelectedQuote]
findByQuoteId quoteId = buildDType $ do
  selQuotesT <- Esq.findAll' $ do
    selQuote <- from $ table @SelectedQuoteT
    where_ $
      selQuote ^. SelectedQuoteQuoteId ==. val (toKey quoteId)
    return selQuote
  catMaybes <$> mapM buildFullSelectedQuote selQuotesT

findById :: Transactionable m => Id SelectedQuote -> m (Maybe SelectedQuote)
findById selQuoteId = buildDType $
  runMaybeT $ do
    selQuoteT <- MaybeT $ Esq.findById' selQuoteId
    MaybeT $ buildFullSelectedQuote selQuoteT
