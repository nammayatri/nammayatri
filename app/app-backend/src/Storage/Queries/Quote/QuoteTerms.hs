module Storage.Queries.Quote.QuoteTerms where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote
import Domain.Types.Quote.QuoteTerms
import Storage.Tabular.Quote.QuoteTerms

findAllByQuoteId :: Transactionable m => Id Quote -> m [QuoteTermsEntity]
findAllByQuoteId quoteId =
  Esq.findAll $ do
    quoteTerms <- from $ table @QuoteTermsT
    where_ $ quoteTerms ^. QuoteTermsQuoteId ==. val (getId quoteId)
    return quoteTerms
