module Domain.Action.UI.Quotes where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Search as DSearch
import Storage.Queries.Quote as QQuote

newtype GetQuotesRes = GetQuotesRes
  { quotes :: [DQuote.QuoteAPIEntity]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, PrettyShow)

getQuotesHandler :: EsqDBReplicaFlow m r => Id DSearch.Search -> m GetQuotesRes
getQuotesHandler searchId = do
  quoteAggregates <- QQuote.findAllAggregatesBySearchId searchId
  let quoteAPIEntities = map makeQuoteAPIEntity quoteAggregates
  return $ GetQuotesRes quoteAPIEntities
  where
    makeQuoteAPIEntity (quote, depStation, arrStation) =
      DQuote.makeQuoteAPIEntity quote depStation arrStation
