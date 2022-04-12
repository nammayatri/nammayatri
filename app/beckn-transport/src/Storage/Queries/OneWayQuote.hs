module Storage.Queries.OneWayQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.OneWayQuote
import Domain.Types.Quote
import Storage.Tabular.OneWayQuote

create :: OneWayQuote -> SqlDB ()
create = create'

findByQuoteId :: Transactionable m => Id Quote -> m (Maybe OneWayQuote)
findByQuoteId quoteId =
  findOne $ do
    oneWayQuote <- from $ table @OneWayQuoteT
    where_ $ oneWayQuote ^. OneWayQuoteQuoteId ==. val (toKey quoteId)
    return oneWayQuote
