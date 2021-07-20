module Product.Quote where

import App.Types
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Types.API.Quote as API
import Types.Error
import qualified Types.ProductInfo as Info
import qualified Types.Storage.SearchRequest as SSR
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.SearchReqLocation as Location
import Utils.Common

getQuotes :: Id SSR.SearchRequest -> Id Person.Person -> FlowHandler API.GetQuotesRes
getQuotes searchRequestId _ = withFlowHandlerAPI $ do
  searchRequest <- QSR.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  fromLocation <- Location.findLocationById searchRequest.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- Location.findLocationById searchRequest.toLocationId >>= fromMaybeM LocationNotFound
  quoteList <- QQuote.findAllByRequestId searchRequest.id
  quotes <- traverse buildQuote $ sortByNearestDriverDistance quoteList
  return $
    API.GetQuotesRes
      { fromLocation = Location.makeSearchReqLocationAPIEntity fromLocation,
        toLocation = Location.makeSearchReqLocationAPIEntity toLocation,
        quotes
      }
  where
    sortByNearestDriverDistance quoteList = do
      let sortFunc = \(_, aDist) (_, bDist) -> do
            compare aDist bDist
      fmap fst $ sortBy sortFunc $ fmap (\quote -> (quote, fromMaybe (0 :: Double) $ readMaybe . T.unpack =<< quote.udf1)) quoteList
    buildQuote :: DBFlow m r => SQuote.Quote -> m SQuote.QuoteAPIEntity
    buildQuote quote = do
      info :: Info.ProductInfo <- quote.info >>= decodeFromText & fromMaybeM (InternalError "Unable to read product info.")
      return $
        SQuote.QuoteAPIEntity
          { id = quote.id,
            estimatedPrice = fromMaybe 0 quote.price,
            agencyName = fromMaybe "" $ info.provider >>= (.name),
            agencyNumber = fromMaybe "" $ info.provider >>= (listToMaybe . (.phones)),
            agencyCompletedRidesCount = fromMaybe 0 $ info.provider >>= (.info) >>= (.completed),
            nearestDriverDistance = fromMaybe 0 $ readMaybe . T.unpack =<< quote.udf1,
            createdAt = quote.createdAt
          }
