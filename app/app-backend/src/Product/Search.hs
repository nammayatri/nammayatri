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
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Product.Location as Location (getDistance)
import Product.Serviceability
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.API.Search as API
import Types.API.Serviceability
import qualified Types.Common as Common
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.ProductInfo
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as Person
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.Common
import qualified Utils.Metrics as Metrics
import qualified Data.Text as T

search :: Id Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  validateServiceability
  fromLocation <- Location.buildSearchReqLoc req.origin
  toLocation <- Location.buildSearchReqLoc req.destination
  now <- getCurrentTime
  searchRequest <- mkSearchRequest req (getId personId) fromLocation toLocation now
  Metrics.incrementSearchRequestCount
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics txnId
  DB.runSqlDBTransaction $ do
    Location.create fromLocation
    Location.create toLocation
    QSearchRequest.create searchRequest
  env <- ask
  let bapNwAddr = env.bapNwAddress
  context <- buildContext "search" txnId (Just bapNwAddr) Nothing
  distance <- Location.getDistance (req.origin.gps) (req.destination.gps)
  let intent = mkIntent req now
      tags = Just [Tag "distance" (maybe  "" show distance)]
  fork "search" . withRetry $
    ExternalAPI.search (xGatewayUri env) (Search.SearchReq context $ Search.SearchIntent (intent & #tags .~ tags))
  return . API.SearchRes $ searchRequest.id
  where
    validateServiceability = do
      let originGps = req.origin.gps
      let destinationGps = req.destination.gps
      let serviceabilityReq = RideServiceabilityReq originGps destinationGps
      unlessM (rideServiceable serviceabilityReq) $
        throwError $ ProductNotServiceable "due to georestrictions"

searchCb ::
  SignatureAuthResult Org.Organization ->
  SignatureAuthResult Org.Organization ->
  Search.OnSearchReq ->
  FlowHandler Search.OnSearchRes
searchCb _ _ req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_search" $ req.context
    Metrics.finishSearchMetrics $ req.context.transaction_id
    case req.contents of
      Right msg -> do
        let catalog = msg.catalog
        searchCbService req catalog
      Left err -> logTagError "on_search req" $ "on_search error: " <> show err
    return Ack

searchCbService :: (HasFlowEnv m r '["searchConfirmExpiry" ::: Maybe Seconds], DBFlow m r) => Search.OnSearchReq -> BM.Catalog -> m ()
searchCbService req catalog = do
  let searchRequestId = Id $ req.context.transaction_id --searchRequestId $ service.id
  searchRequest <- QSearchRequest.findById searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  bpp <-
    Org.findOrganizationByCallbackUri (req.context.bpp_uri) Org.PROVIDER
      >>= fromMaybeM OrgDoesNotExist
  let personId = searchRequest.requestorId
  DB.runSqlDBTransaction =<< case (catalog.categories, catalog.items) of
    ([], _) -> throwError $ InvalidRequest "Missing provider"
    (category : _, []) -> do
      let provider = fromBeckn category
      declinedPI <- mkDeclinedQuote searchRequest bpp provider personId
      return $ QQuote.create declinedPI
    (category : _, items) -> do
      let provider = fromBeckn category
      quotes <- traverse (mkQuote searchRequest bpp provider personId) items
      return $ do
        traverse_ QQuote.create quotes

mkSearchRequest ::
  ( (HasFlowEnv m r ["searchRequestExpiry" ::: Maybe Seconds, "graphhopperUrl" ::: BaseUrl]),
    DBFlow m r,
    CoreMetrics m
  ) =>
  API.SearchReq ->
  Text ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  UTCTime ->
  m SearchRequest.SearchRequest
mkSearchRequest req userId from to now = do
  searchRequestId <- generateGUID
  validTill <- getSearchRequestExpiry now
  let vehVar = req.vehicle
  return
    SearchRequest.SearchRequest
      { id = searchRequestId,
        startTime = now,
        validTill = validTill,
        requestorId = Id userId,
        fromLocationId = from.id,
        toLocationId = to.id,
        vehicleVariant = vehVar,
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
  Id Person.Person ->
  Core.Item ->
  m SQuote.Quote
mkQuote searchRequest bppOrg provider personId item = do
  now <- getCurrentTime
  let info = ProductInfo (Just provider) Nothing
  price <- item.price.listed_value >>= convertDecimalValueToAmount & fromMaybeM (InternalError "Unable to parse price")
  nearestDriverDist <- getNearestDriverDist
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where searchRequest.startTime != product.startTime, etc
  return
    SQuote.Quote
      { id = Id $ item.id,
        shortId = "",
        requestId = searchRequest.id,
        personId = Just personId,
        personUpdatedAt = Nothing,
        quantity = 1,
        entityType = SQuote.VEHICLE,
        status = SQuote.INSTOCK,
        startTime = searchRequest.startTime,
        endTime = Nothing,
        validTill = searchRequest.validTill,
        actualDistance = Nothing,
        entityId = Nothing,
        price = price,
        actualPrice = Nothing,
        distanceToNearestDriver = nearestDriverDist,
        providerMobileNumber = "UNKNOWN",
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        fromLocation = Just $ searchRequest.fromLocationId,
        toLocation = Just $ searchRequest.toLocationId,
        info = Just $ encodeToText info,
        providerId = bppOrg.id,
        createdAt = now,
        updatedAt = now
      }
  where
    getNearestDriverDist = do
      let dist = (.value) <$> listToMaybe (filter (\tag -> tag.key == "nearestDriverDist") item.tags)
      (readMaybe . T.unpack =<< dist) & fromMaybeM (InternalError "Unable to parse nearestDriverDist")

mkDeclinedQuote :: MonadFlow m => SearchRequest.SearchRequest -> Org.Organization -> Common.Provider -> Id Person.Person -> m SQuote.Quote
mkDeclinedQuote searchRequest bppOrg provider personId = do
  now <- getCurrentTime
  quoteId <- generateGUID
  let info = ProductInfo (Just provider) Nothing
  return
    SQuote.Quote
      { id = Id quoteId,
        shortId = "",
        requestId = searchRequest.id,
        personId = Just personId,
        personUpdatedAt = Nothing,
        quantity = 1,
        entityType = SQuote.VEHICLE,
        status = SQuote.OUTOFSTOCK,
        startTime = searchRequest.startTime,
        endTime = Nothing,
        validTill = searchRequest.validTill,
        actualDistance = Nothing,
        entityId = Nothing,
        price = 0,
        actualPrice = Nothing,
        distanceToNearestDriver = 0,
        providerMobileNumber = "UNKNOWN",
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        fromLocation = Just $ searchRequest.fromLocationId,
        toLocation = Just $ searchRequest.toLocationId,
        info = Just $ encodeToText info,
        providerId = bppOrg.id,
        createdAt = now,
        updatedAt = now
      }

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
      vehicle =
        emptyVehicle
          { variant = show req.vehicle
          }
      fare = emptyPrice
  Intent
    { query_string = Nothing,
      provider_id = Nothing,
      category_id = Nothing,
      item_id = Nothing,
      tags = Nothing,
      pickups = [pickup],
      drops = [drop'],
      vehicle = vehicle,
      payload = Payload Nothing Nothing [] Nothing,
      transfer = Nothing,
      fare = fare
    }
