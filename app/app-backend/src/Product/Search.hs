{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import App.Types
import Beckn.Product.Validation.Context (validateContext)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Migration.API.Search as Core9
import qualified Beckn.Types.Core.Migration.API.Types as Core9
import qualified Beckn.Types.Core.Migration.Context as Core9
import qualified Beckn.Types.Core.Migration.Gps as Mig
import qualified Beckn.Types.Core.Migration.Intent as Mig
import qualified Beckn.Types.Core.Migration.Location as Mig
import qualified Beckn.Types.Core.ReqTypes as Common
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.Common.Context as Common
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.Search as Search
import Beckn.Types.Id
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Control.Lens ((?~))
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Product.Location as Location (getDistance)
import Product.MetroOffer (buildContextMetro)
import Product.Serviceability
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.API.Search as API
import Types.API.Serviceability
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.Common
import qualified Utils.Metrics as Metrics

search :: Id Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  validateServiceability
  fromLocation <- Location.buildSearchReqLoc req.origin
  toLocation <- Location.buildSearchReqLoc req.destination
  now <- getCurrentTime
  distance <-
    Location.getDistance req.origin.gps req.destination.gps
      >>= fromMaybeM (InternalError "Unable to count distance.")
  searchRequest <- buildSearchRequest (getId personId) fromLocation toLocation distance now
  Metrics.incrementSearchRequestCount
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics txnId
  DB.runSqlDBTransaction $ do
    Location.create fromLocation
    Location.create toLocation
    QSearchRequest.create searchRequest
  bapURIs <- asks (.bapSelfURIs)
  fork "search" . withRetry $ do
    fork "search cabs" $ do
      context <- buildTaxiContext txnId bapURIs.cabs Nothing
      let intent = mkIntent req now distance
      void $ ExternalAPI.search (Common.BecknReq context $ Search.SearchMessage intent)
    fork "search metro" $ do
      contextMig <- buildContextMetro Core9.SEARCH txnId bapURIs.metro Nothing
      intentMig <- mkIntentMig req
      ExternalAPI.searchMetro (Core9.BecknReq contextMig $ Core9.SearchIntent intentMig)
  return . API.SearchRes $ searchRequest.id
  where
    validateServiceability = do
      let originGps = req.origin.gps
      let destinationGps = req.destination.gps
      let serviceabilityReq = RideServiceabilityReq originGps destinationGps
      unlessM (rideServiceable serviceabilityReq) $
        throwError $ ProductNotServiceable "due to georestrictions"

searchCb ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReq ->
  FlowHandler AckResponse
searchCb _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext $ req.context
  Metrics.finishSearchMetrics $ req.context.transaction_id
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      searchCbService req.context catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

type SearchCbFlow m r = (HasFlowEnv m r '["searchConfirmExpiry" ::: Maybe Seconds], DBFlow m r)

searchCbService :: SearchCbFlow m r => Common.Context -> OnSearch.Catalog -> m ()
searchCbService context catalog = do
  let searchRequestId = Id $ context.transaction_id
  searchRequest <- QSearchRequest.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  bpp <-
    Org.findOrganizationByCallbackUri context.bpp_uri Org.PROVIDER
      >>= fromMaybeM OrgDoesNotExist
  case catalog.bpp_providers of
    [] -> throwError $ InvalidRequest "Missing provider"
    (provider : _) -> do
      let items = provider.items
      quotes <- traverse (buildQuote searchRequest bpp provider) items
      DB.runSqlDBTransaction $ traverse_ QQuote.create quotes

buildSearchRequest ::
  ( (HasFlowEnv m r ["searchRequestExpiry" ::: Maybe Seconds, "graphhopperUrl" ::: BaseUrl]),
    DBFlow m r,
    CoreMetrics m
  ) =>
  Text ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  Double ->
  UTCTime ->
  m SearchRequest.SearchRequest
buildSearchRequest userId from to distance now = do
  searchRequestId <- generateGUID
  validTill <- getSearchRequestExpiry now
  return
    SearchRequest.SearchRequest
      { id = searchRequestId,
        startTime = now,
        validTill = validTill,
        requestorId = Id userId,
        fromLocationId = from.id,
        toLocationId = to.id,
        distance = distance,
        createdAt = now
      }
  where
    getSearchRequestExpiry :: (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getSearchRequestExpiry startTime = do
      searchRequestExpiry <- maybe 7200 fromIntegral <$> asks (.searchRequestExpiry)
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger searchRequestExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

buildQuote ::
  MonadFlow m =>
  SearchRequest.SearchRequest ->
  Org.Organization ->
  OnSearch.Provider ->
  OnSearch.Item ->
  m SQuote.Quote
buildQuote searchRequest bppOrg provider item = do
  now <- getCurrentTime
  uid <- generateGUID
  return
    SQuote.Quote
      { id = uid,
        bppQuoteId = Id item.id,
        requestId = searchRequest.id,
        estimatedFare = realToFrac item.estimated_price.value,
        estimatedTotalFare = realToFrac item.discounted_price.value,
        discount = realToFrac <$> (item.discount <&> (.value)),
        distanceToNearestDriver = realToFrac item.nearest_driver_distance,
        providerMobileNumber = provider.contacts,
        providerName = provider.name,
        providerCompletedRidesCount = provider.rides_completed,
        providerId = bppOrg.id,
        vehicleVariant = item.vehicle_variant,
        createdAt = now
      }

mkIntent :: API.SearchReq -> UTCTime -> Double -> Search.Intent
mkIntent req startTime distance = do
  let startLocation =
        Search.StartInfo
          { location =
              Search.Location $
                Search.Gps
                  { lat = req.origin.gps.lat,
                    lon = req.origin.gps.lon
                  },
            time = Search.Time startTime
          }
      endLocation =
        Search.StopInfo
          { location =
              Search.Location $
                Search.Gps
                  { lat = req.destination.gps.lat,
                    lon = req.destination.gps.lon
                  }
          }
      fulfillment =
        Search.FulFillmentInfo
          { distance = realToFrac distance,
            start = startLocation,
            end = endLocation
          }
  Search.Intent
    { ..
    }

mkIntentMig :: (MonadThrow m, Log m) => API.SearchReq -> m Mig.Intent
mkIntentMig req = do
  from <- stopToLoc req.origin
  to <- stopToLoc req.destination
  pure $
    Mig.emptyIntent
      & #fulfillment
        ?~ ( Mig.emptyFulFillmentInfo
               & #start
                 ?~ Mig.LocationAndTime
                   { location = Just from,
                     time = Nothing
                   }
               & #end
                 ?~ Mig.LocationAndTime
                   { location = Just to,
                     time = Nothing
                   }
           )
  where
    stopToLoc Location.SearchReqLocationAPIEntity {gps} = do
      gps' <-
        fromMaybeM (InvalidRequest "bad coordinates") $
          Mig.Gps
            <$> Just gps.lat
            <*> Just gps.lon
      pure $ Mig.emptyLocation & #gps ?~ gps'
