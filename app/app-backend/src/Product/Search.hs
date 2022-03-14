{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Product.Validation.Context (validateContext)
import Beckn.Serviceability
import Beckn.Storage.Esqueleto (runTransaction)
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch
import Beckn.Streaming.MonadProducer
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Migration.API.Search as Core9
import qualified Beckn.Types.Core.Migration.Context as Core9
import qualified Beckn.Types.Core.Migration.Gps as Mig
import qualified Beckn.Types.Core.Migration.Intent as Mig
import qualified Beckn.Types.Core.Migration.Location as Mig
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.Common.Context as Common
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.Search as Search
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Control.Lens ((?~))
import Domain.Types.OnSearchEvent
import qualified Domain.Types.Person as Person
import Domain.Types.Quote (Quote)
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.SearchReqLocation as Location
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Flow as ExternalAPI
import Product.MetroOffer (buildContextMetro)
import Storage.Queries.Geometry
import qualified Storage.Queries.OnSearchEvent as OnSearchEvent
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Metrics (CoreMetrics)
import qualified Tools.Metrics as Metrics
import qualified Types.API.Search as API
import Types.Error
import Utils.Common

search :: Id Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  validateServiceability
  fromLocation <- buildSearchReqLoc req.origin
  toLocation <- buildSearchReqLoc req.destination
  now <- getCurrentTime
  distance <- (.info.distance) <$> MapSearch.getDistance (Just MapSearch.CAR) req.origin.gps req.destination.gps
  searchRequest <- buildSearchRequest (getId personId) fromLocation toLocation distance now
  Metrics.incrementSearchRequestCount
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics txnId
  DB.runTransaction $ do
    Location.create fromLocation
    Location.create toLocation
    QSearchRequest.create searchRequest
  bapURIs <- askConfig (.bapSelfURIs)
  bapIDs <- askConfig (.bapSelfIds)
  fork "search cabs" . withRetry $ do
    context <- buildTaxiContext Core9.SEARCH txnId bapIDs.cabs bapURIs.cabs Nothing Nothing
    let intent = mkIntent req now
    void $ ExternalAPI.search (BecknReq context $ Search.SearchMessage intent)
  fork "search metro" . withRetry $ do
    contextMig <- buildContextMetro Core9.SEARCH txnId bapIDs.metro bapURIs.metro
    intentMig <- mkIntentMig req
    ExternalAPI.searchMetro (BecknReq contextMig $ Core9.SearchIntent intentMig)
  fork "search public-transport" $ sendPublicTransportSearchRequest personId searchRequest.id req now
  return . API.SearchRes $ searchRequest.id
  where
    validateServiceability = do
      unlessM (rideServiceable someGeometriesContain req.origin.gps req.destination.gps) $
        throwError RideNotServiceable

sendPublicTransportSearchRequest ::
  MonadProducer PublicTransportSearch m =>
  Id Person.Person ->
  Id SearchRequest.SearchRequest ->
  API.SearchReq ->
  UTCTime ->
  m ()
sendPublicTransportSearchRequest personId searchRequestId req now = do
  producePublicTransportSearchMessage publicTransportSearch
  where
    publicTransportSearch =
      PublicTransportSearch
        { id = getId searchRequestId,
          gps = req.origin.gps,
          requestorId = getId personId,
          createdAt = now
        }

logOnSearchEvent :: EsqDBFlow m r => OnSearch.OnSearchReq -> m ()
logOnSearchEvent (BecknCallbackReq context (leftToMaybe -> mbErr)) = do
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing context.bpp_id")
  let transactionId = context.transaction_id
  let errorType = show . (._type) <$> mbErr
  let errorCode = (.code) <$> mbErr
  let errorMessage = (.message) =<< mbErr
  runTransaction $
    OnSearchEvent.create $ OnSearchEvent {..}

searchCb ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReq ->
  FlowHandler AckResponse
searchCb _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext $ req.context
  Metrics.finishSearchMetrics $ req.context.transaction_id
  logOnSearchEvent req
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      searchCbService req.context catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

type SearchCbFlow m r = (HasFlowEnv m r '["searchConfirmExpiry" ::: Maybe Seconds], EsqDBFlow m r)

searchCbService :: SearchCbFlow m r => Common.Context -> OnSearch.Catalog -> m ()
searchCbService context catalog = do
  let searchRequestId = Id $ context.transaction_id
  searchRequest <- QSearchRequest.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
  case catalog.bpp_providers of
    [] -> throwError $ InvalidRequest "Missing bpp/providers" -- TODO: make it NonEmpty
    (provider : _) -> do
      let items = provider.items
      quotes <- traverse (buildQuote searchRequest providerId providerUrl provider) items
      whenJustM (duplicateCheck quotes searchRequest.id providerId) (\_ -> throwError $ InvalidRequest "Duplicate OnSearch request")
      DB.runTransaction $ traverse_ QQuote.create quotes
  where
    duplicateCheck :: EsqDBFlow m r => [Quote] -> Id SearchRequest -> Text -> m (Maybe Quote)
    duplicateCheck [] _ _ = return Nothing
    duplicateCheck (quote_ : _) txnId_ bppId_ =
      QQuote.findByTxnIdAndBppIdAndQuoteId txnId_ bppId_ quote_.bppQuoteId

buildSearchRequest ::
  ( (HasFlowEnv m r ["searchRequestExpiry" ::: Maybe Seconds, "graphhopperUrl" ::: BaseUrl]),
    MonadFlow m,
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
        riderId = Id userId,
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
  Text ->
  BaseUrl ->
  OnSearch.Provider ->
  OnSearch.Item ->
  m SQuote.Quote
buildQuote searchRequest providerId providerUrl provider item = do
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
        providerId,
        providerUrl,
        vehicleVariant = item.vehicle_variant,
        createdAt = now
      }

mkIntent :: API.SearchReq -> UTCTime -> Search.Intent
mkIntent req startTime = do
  let startLocation =
        Search.StartInfo
          { location = mkLocation req.origin,
            time = Search.Time startTime
          }
      endLocation =
        Search.StopInfo
          { location = mkLocation req.destination
          }
      fulfillment =
        Search.FulFillmentInfo
          { start = startLocation,
            end = endLocation
          }
  Search.Intent
    { ..
    }
  where
    mkLocation info =
      Search.Location
        { gps =
            Search.Gps
              { lat = info.gps.lat,
                lon = info.gps.lon
              },
          address = do
            let API.SearchReqAddress {..} = info.address
            Search.Address
              { area_code = areaCode,
                ..
              }
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
    stopToLoc API.SearchReqLocation {gps} = do
      let gps' = Mig.Gps gps.lat gps.lon
      pure $ Mig.emptyLocation & #gps ?~ gps'

buildSearchReqLoc :: MonadFlow m => API.SearchReqLocation -> m Location.SearchReqLocation
buildSearchReqLoc API.SearchReqLocation {..} = do
  now <- getCurrentTime
  locId <- generateGUID
  return
    Location.SearchReqLocation
      { id = locId,
        lat = gps.lat,
        lon = gps.lon,
        city = address.city,
        state = address.state,
        country = address.country,
        street = address.street,
        door = address.door,
        building = address.building,
        areaCode = address.areaCode,
        area = address.area,
        createdAt = now,
        updatedAt = now
      }
