{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote.OneWayQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Storage.Tabular.Quote ()
import Storage.Tabular.Quote.OneWayQuote

findByQuoteId' :: Transactionable m => Id Quote -> DTypeBuilder m (Maybe OneWayQuoteT)
findByQuoteId' quoteId =
  Esq.findOne' $ do
    oneWayQuote <- from $ table @OneWayQuoteT
    where_ $ oneWayQuote ^. OneWayQuoteQuoteId ==. val (toKey quoteId)
    return oneWayQuote
