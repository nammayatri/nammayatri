{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote.RentalQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.Quote.RentalQuote
import Storage.Tabular.Quote.RentalQuote

findByQuoteId :: Transactionable m => Id Quote -> m (Maybe RentalQuote)
findByQuoteId quoteId =
  Esq.findOne $ do
    rentalQuote <- from $ table @RentalQuoteT
    where_ $ rentalQuote ^. RentalQuoteQuoteId ==. val (getId quoteId)
    return rentalQuote
