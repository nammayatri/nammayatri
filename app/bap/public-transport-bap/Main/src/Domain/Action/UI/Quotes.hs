module Domain.Action.UI.Quotes where

import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Search as DSearch
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.GenericPretty (PrettyShow)
import Storage.Queries.Quote as QQuote

newtype GetQuotesRes = GetQuotesRes
  { quotes :: [DQuote.QuoteAPIEntity]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, PrettyShow)

getQuotesHandler :: EsqDBReplicaFlow m r => Id DSearch.Search -> m GetQuotesRes
getQuotesHandler searchId = do
  quoteAggregates <- runInReplica $ QQuote.findAllAggregatesBySearchId searchId
  let quoteAPIEntities = map makeQuoteAPIEntity quoteAggregates
  return $ GetQuotesRes quoteAPIEntities
  where
    makeQuoteAPIEntity (quote, depStation, arrStation) =
      DQuote.makeQuoteAPIEntity quote depStation arrStation
