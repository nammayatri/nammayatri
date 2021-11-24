{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Search as Search
import Beckn.Types.Core.Ack
import Beckn.Types.Core.DecimalValue (convertDecimalValueToAmount)
import qualified Beckn.Types.Core.Item as Core
import Beckn.Types.Core.Location
import qualified Beckn.Types.Core.Migration.API.Search as Core9
import qualified Beckn.Types.Core.Migration.API.Types as Core9
import qualified Beckn.Types.Core.Migration.Context as Core9
import qualified Beckn.Types.Core.Migration.Gps as Mig
import qualified Beckn.Types.Core.Migration.Intent as Mig
import qualified Beckn.Types.Core.Migration.Location as Mig
import Beckn.Types.Core.Price
import Beckn.Types.Core.Tag
import Beckn.Types.Id
import Beckn.Types.Mobility.Catalog as BM
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Payload
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Control.Lens ((?~))
import qualified Data.Text as T
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
import qualified Types.Common as Common
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
    fork "search 0.8" $ do
      context <- buildMobilityContext "search" txnId (Just bapURIs.cabs) Nothing
      let intent = mkIntent req now
          tags = Just [Tag "distance" $ show distance]
      ExternalAPI.search (Search.SearchReq context $ Search.SearchIntent (intent & #tags .~ tags))
    fork "search 0.9" $ do
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
  Search.OnSearchReq ->
  FlowHandler Search.OnSearchRes
searchCb _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext "on_search" $ req.context
  Metrics.finishSearchMetrics $ req.context.transaction_id
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      searchCbService req catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

type SearchCbFlow m r = (HasFlowEnv m r '["searchConfirmExpiry" ::: Maybe Seconds], DBFlow m r)

searchCbService :: SearchCbFlow m r => Search.OnSearchReq -> BM.Catalog -> m ()
searchCbService req catalog = do
  let searchRequestId = Id $ req.context.transaction_id --searchRequestId $ service.id
  searchRequest <- QSearchRequest.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  bpp <-
    Org.findOrganizationByCallbackUri (req.context.bpp_uri) Org.PROVIDER
      >>= fromMaybeM OrgDoesNotExist
  case (catalog.categories, catalog.items) of
    ([], _) -> throwError $ InvalidRequest "Missing provider"
    (_ : _, []) -> return ()
    (category : _, items) -> do
      let provider = fromBeckn category
      quotes <- traverse (mkQuote searchRequest bpp provider) items
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

mkQuote ::
  MonadFlow m =>
  SearchRequest.SearchRequest ->
  Org.Organization ->
  Common.Provider ->
  Core.Item ->
  m SQuote.Quote
mkQuote searchRequest bppOrg provider item = do
  now <- getCurrentTime
  estimatedFare <-
    item.price.listed_value >>= convertDecimalValueToAmount
      & fromMaybeM (InternalError "Unable to parse price")
  estimatedTotalPrice <-
    item.totalPrice.listed_value >>= convertDecimalValueToAmount & fromMaybeM (InternalError "Unable to parse estimated total price")
  nearestDriverDist <- getNearestDriverDist
  vehicleVariant <- item.descriptor.code & fromMaybeM (InvalidRequest "Missing item.descriptor.code")
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where searchRequest.startTime != product.startTime, etc
  return
    SQuote.Quote
      { id = Id $ item.id,
        requestId = searchRequest.id,
        estimatedFare = estimatedFare,
        estimatedTotalFare = estimatedTotalPrice,
        discount = item.discount,
        distanceToNearestDriver = nearestDriverDist,
        providerMobileNumber = fromMaybe "UNKNOWN" $ listToMaybe provider.phones,
        providerName = fromMaybe "" provider.name,
        providerCompletedRidesCount = fromMaybe 0 $ provider.info >>= (.completed),
        providerId = bppOrg.id,
        vehicleVariant,
        createdAt = now
      }
  where
    getNearestDriverDist = do
      let dist = (.value) <$> listToMaybe (filter (\tag -> tag.key == "nearestDriverDist") item.tags)
      (readMaybe . T.unpack =<< dist) & fromMaybeM (InternalError "Unable to parse nearestDriverDist")

mkIntent :: API.SearchReq -> UTCTime -> Intent
mkIntent req now = do
  let pickupLocation =
        emptyLocation
          { gps = Just $ toBeckn req.origin.gps,
            address = Just $ toBeckn req.origin.address
          }
      dropLocation =
        emptyLocation
          { gps = Just $ toBeckn req.destination.gps,
            address = Just $ toBeckn req.destination.address
          }
      pickup =
        Stop
          { id = "",
            descriptor = Nothing,
            location = pickupLocation,
            arrival_time = StopTime now Nothing,
            departure_time = StopTime now Nothing,
            transfers = []
          }
      drop' =
        Stop
          { id = "",
            descriptor = Nothing,
            location = dropLocation,
            arrival_time = StopTime now Nothing,
            departure_time = StopTime now Nothing,
            transfers = []
          }
  Intent
    { query_string = Nothing,
      provider_id = Nothing,
      category_id = Nothing,
      item_id = Nothing,
      tags = Nothing,
      pickups = [pickup],
      drops = [drop'],
      vehicle = emptyVehicle,
      payload = Payload Nothing Nothing [] Nothing,
      transfer = Nothing,
      fare = emptyPrice
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
      let GPS {lat, lon} = toBeckn gps
      gps' <-
        fromMaybeM (InvalidRequest "bad coordinates") $
          Mig.Gps
            <$> readMaybe (T.unpack lat)
            <*> readMaybe (T.unpack lon)
      pure $ Mig.emptyLocation & #gps ?~ gps'
