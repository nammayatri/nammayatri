module Product.Quotes where

import API.UI.SearchId.Quotes.Types as Quotes
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Data.Maybe (catMaybes)
import Domain.PublicTranport as DPublicTranport
import Domain.Quote as DQuote
import qualified Domain.Search as DSearch
import Storage.Queries.PublicTranport as QPublicTransport
import Storage.Queries.Quote as QQuote

getQuotesHandler :: EsqDBFlow m r => Id DSearch.Search -> m Quotes.GetQuotesRes
getQuotesHandler searchId = do
  quotes <- QQuote.findAllBySearchId searchId
  transportStations <- QPublicTransport.findAll
  let mbQuoteAPIEntities = map (mkQuote transportStations) quotes
  return $ Quotes.GetQuotesRes (catMaybes mbQuoteAPIEntities)

mkQuote :: [DPublicTranport.PublicTranport] -> DQuote.Quote -> Maybe DQuote.QuoteAPIEntity
mkQuote transportStations quote = do
  departureStation <- find (\location -> location.id == quote.departureStationId) transportStations
  arrivalStation <- find (\location -> location.id == quote.arrivalStationId) transportStations
  pure $ DQuote.makeQuoteAPIEntity quote departureStation arrivalStation