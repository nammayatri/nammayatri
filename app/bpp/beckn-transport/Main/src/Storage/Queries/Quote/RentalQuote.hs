{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote.RentalQuote where

import Domain.Types.Quote
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Quote ()
import Storage.Tabular.Quote.RentalQuote

findByQuoteId' :: Transactionable m => Id Quote -> DTypeBuilder m (Maybe RentalQuoteT)
findByQuoteId' quoteId =
  Esq.findOne' $ do
    rentalQuote <- from $ table @RentalQuoteT
    where_ $ rentalQuote ^. RentalQuoteQuoteId ==. val (toKey quoteId)
    return rentalQuote
