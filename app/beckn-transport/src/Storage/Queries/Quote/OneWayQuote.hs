{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote.OneWayQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.Quote.OneWayQuote
import Storage.Tabular.Quote.OneWayQuote

findByQuoteId :: Transactionable m => Id Quote -> m (Maybe OneWayQuoteEntity)
findByQuoteId quoteId =
  findOne $ do
    oneWayQuote <- from $ table @OneWayQuoteT
    where_ $ oneWayQuote ^. OneWayQuoteQuoteId ==. val (getId quoteId)
    return oneWayQuote
