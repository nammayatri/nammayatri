module Product.Quotes where

import API.UI.SearchId.Quotes.Types as Quotes
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Search as Domain
import Storage.Queries.Quote as QQuote

getQuotesHandler :: EsqDBFlow m r => Id Domain.Search -> m Quotes.GetQuotesRes
getQuotesHandler searchId = do
  quoteAggregates <- QQuote.findAllAggregatesBySearchId searchId
  let quoteAPIEntities = map makeQuoteAPIEntity quoteAggregates
  return $ Quotes.GetQuotesRes quoteAPIEntities
  where
    makeQuoteAPIEntity (quote, depStation, arrStation) =
      Domain.makeQuoteAPIEntity quote depStation arrStation
