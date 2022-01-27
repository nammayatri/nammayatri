module Product.Quotes where

import API.UI.SearchId.Quotes.Types as Quotes
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Data.Maybe (catMaybes)
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Search as Domain
import qualified Domain.Types.TransportStation as Domain
import Storage.Queries.Quote as QQuote
import Storage.Queries.TransportStation as QTransportStation

getQuotesHandler :: EsqDBFlow m r => Id Domain.Search -> m Quotes.GetQuotesRes
getQuotesHandler searchId = do
  quotes <- QQuote.findAllBySearchId searchId
  transportStations <- QTransportStation.findAll
  let mbQuoteAPIEntities = map (mkQuote transportStations) quotes
  return $ Quotes.GetQuotesRes (catMaybes mbQuoteAPIEntities)

mkQuote :: [Domain.TransportStation] -> Domain.Quote -> Maybe Domain.QuoteAPIEntity
mkQuote transportStations quote = do
  departureStation <- find (\location -> location.id == quote.departureStationId) transportStations
  arrivalStation <- find (\location -> location.id == quote.arrivalStationId) transportStations
  pure $ Domain.makeQuoteAPIEntity quote departureStation arrivalStation
