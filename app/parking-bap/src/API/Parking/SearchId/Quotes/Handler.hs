module API.Parking.SearchId.Quotes.Handler where

import qualified API.Parking.SearchId.Quotes.Types as Quotes
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Error (withFlowHandlerAPI)
import Data.Maybe (catMaybes)
import qualified Domain.ParkingLocation as DParkingLocation
import qualified Domain.Quote as DQuote
import qualified Domain.Search as DSearch
import qualified Storage.Queries.ParkingLocation as QParkingLocation
import qualified Storage.Queries.Quote as QQuote
import Tools.Auth (PersonId)

-- TODO Do we need to check that personId == search.requestorId?
handler :: Id DSearch.Search -> PersonId -> FlowHandler Quotes.GetQuotesRes
handler searchId _personId = withFlowHandlerAPI $ do
  --TODO: next two queries replace with join query
  quotes <- QQuote.findAllBySearchId searchId
  parkingLocations <- QParkingLocation.findAll
  let mbQuoteAPIEntities = map (mkQuote parkingLocations) quotes
  return $ Quotes.GetQuotesRes (catMaybes mbQuoteAPIEntities)

mkQuote :: [DParkingLocation.ParkingLocation] -> DQuote.Quote -> Maybe DQuote.QuoteAPIEntity
mkQuote locations quote = do
  parkingLocation <- find (\location -> location.id == quote.parkingLocationId) locations
  pure $ DQuote.makeQuoteAPIEntity quote parkingLocation
