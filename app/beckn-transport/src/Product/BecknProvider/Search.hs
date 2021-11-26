module Product.BecknProvider.Search (search) where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Product.Validation.Context
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Migration1.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Migration1.API.Search as Search
import qualified Beckn.Types.Core.Migration1.OnSearch as OnSearch
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Traversable
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Product.BecknProvider.Confirm as Confirm
import Product.FareCalculator
import qualified Product.FareCalculator.Flow as Fare
import qualified Product.Location as Loc
import Servant.Client (showBaseUrl)
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Products as SProduct
import qualified Storage.Queries.Quote as Quote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchReqLocation as Loc
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Types.Error
import Types.Metrics (CoreMetrics, HasBPPMetrics)
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Vehicle as Veh
import Utils.Common
import qualified Utils.Metrics as Metrics

search ::
  Id Org.Organization ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReq ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult _ subscriber) (SignatureAuthResult _ _gateway) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    let context = req.context
    validateContextMig1 context
    transporter <-
      Org.findOrganizationById transporterId
        >>= fromMaybeM OrgDoesNotExist
    callbackUrl <- ExternalAPI.getGatewayUrl
    if not transporter.enabled
      then
        ExternalAPI.withCallback' withRetry transporter "search" OnSearch.onSearchAPI context callbackUrl $
          throwError AgencyDisabled
      else do
        searchMetricsMVar <- Metrics.startSearchMetrics transporterId
        let intent = req.message.intent
        now <- getCurrentTime
        let pickup = intent.fulfillment.start
        let dropOff = intent.fulfillment.end
        let startTime = pickup.time.timestamp
        validity <- getValidTime now startTime
        fromLocation <- buildFromLocation now pickup
        toLocation <- buildFromLocation now dropOff
        let bapOrgId = Id subscriber.subscriber_id
        uuid <- L.generateGUID
        let bapUri = req.context.bap_uri
        let searchRequest = mkSearchRequest req uuid now validity startTime fromLocation toLocation transporterId bapOrgId bapUri
        DB.runSqlDBTransaction $ do
          Loc.create fromLocation
          Loc.create toLocation
          QSearchRequest.create searchRequest
        ExternalAPI.withCallback' withRetry transporter "search" OnSearch.onSearchAPI context callbackUrl $
          onSearchCallback searchRequest transporter fromLocation toLocation searchMetricsMVar
  where
    buildFromLocation now location = do
      let mgps = location.gps
      uuid <- Id <$> L.generateGUID
      pure $
        Location.SearchReqLocation
          { id = uuid,
            lat = mgps.lat,
            long = mgps.lon,
            createdAt = now,
            updatedAt = now
          }

getValidTime :: HasFlowEnv m r '["caseExpiry" ::: Maybe Seconds] => UTCTime -> UTCTime -> m UTCTime
getValidTime now startTime = do
  caseExpiry_ <- maybe 7200 fromIntegral <$> asks (.caseExpiry)
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

mkSearchRequest ::
  Search.SearchReq ->
  Text ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  Id Org.Organization ->
  Id Org.Organization ->
  BaseUrl ->
  SearchRequest.SearchRequest
mkSearchRequest req uuid now validity startTime fromLocation toLocation transporterId bapOrgId bapUri = do
  let bapId = getId bapOrgId
  SearchRequest.SearchRequest
    { id = Id uuid,
      transactionId = req.context.transaction_id,
      startTime = startTime,
      validTill = validity,
      providerId = transporterId,
      requestorId = Id "", -- TODO: Fill this field with Person id.
      fromLocationId = fromLocation.id,
      toLocationId = toLocation.id,
      bapId = bapId,
      bapUri = T.pack $ showBaseUrl bapUri,
      createdAt = now
    }

onSearchCallback ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  SearchRequest.SearchRequest ->
  Org.Organization ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  Metrics.SearchMetricsMVar ->
  m OnSearch.OnSearchMessage
onSearchCallback searchRequest transporter fromLocation toLocation searchMetricsMVar = do
  let transporterId = transporter.id
  pool <- Confirm.calculateDriverPool fromLocation.id transporterId Nothing
  logTagInfo "OnSearchCallback" $
    "Calculated Driver Pool for organization " +|| getId transporterId
      ||+ " with drivers " +| T.intercalate ", " (getId . (.driverId) <$> pool) |+ ""

  let listOfProtoQuotes =
        catMaybes $
          everyPossibleVariant <&> \var ->
            find ((== var) . (.variant)) pool
  -- drivers sorted from nearest to furthest, so with `find`
  -- we take nearest one and calculate fare and make PI for him

  distance <-
    map Loc.locationToLatLong [fromLocation, toLocation]
      & MapSearch.getDistanceMb (Just MapSearch.CAR)
      >>= fromMaybeM CantCalculateDistance

  listOfQuotes <-
    for listOfProtoQuotes $ \poolResult -> do
      fareParams <- calculateFare transporterId poolResult.variant distance searchRequest.startTime
      mkQuote searchRequest fareParams transporterId distance poolResult.distanceToDriver poolResult.variant

  DB.runSqlDBTransaction $
    for_ listOfQuotes Quote.create

  mkOnSearchMessage listOfQuotes transporter
    <* Metrics.finishSearchMetrics transporterId searchMetricsMVar

mkQuote ::
  DBFlow m r =>
  SearchRequest.SearchRequest ->
  Fare.FareParameters ->
  Id Org.Organization ->
  Double ->
  Double ->
  Veh.Variant ->
  m Quote.Quote
mkQuote productSearchRequest fareParams transporterId distance nearestDriverDist vehicleVariant = do
  quoteId <- Id <$> L.generateGUID
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams
      discount = fareParams.discount
      estimatedTotalFare = fareSumWithDiscount fareParams
  products <-
    SProduct.findByName (show vehicleVariant)
      >>= fromMaybeM ProductsNotFound
  return
    Quote.Quote
      { id = quoteId,
        requestId = productSearchRequest.id,
        productId = products.id,
        providerId = transporterId,
        distanceToNearestDriver = nearestDriverDist,
        createdAt = now,
        ..
      }

mkOnSearchMessage ::
  DBFlow m r =>
  [Quote.Quote] ->
  Org.Organization ->
  m OnSearch.OnSearchMessage
mkOnSearchMessage quotes transporterOrg = do
  provider <- buildProvider transporterOrg quotes
  return . OnSearch.OnSearchMessage $ OnSearch.Catalog [provider]

buildProvider :: DBFlow m r => Org.Organization -> [Quote.Quote] -> m OnSearch.Provider
buildProvider org quotes = do
  count <- QRide.getCountByStatus (org.id)
  let tags = mkTags count
      items = map mkItem quotes
  return $
    OnSearch.Provider
      { name = org.name,
        items,
        tags
      }
  where
    mkTags count =
      OnSearch.Tags
        { contacts = fromMaybe "" org.mobileNumber,
          rides_inprogress = fromMaybe 0 $ List.lookup Ride.INPROGRESS count,
          rides_completed = fromMaybe 0 $ List.lookup Ride.COMPLETED count,
          rides_confirmed = fromMaybe 0 $ List.lookup Ride.NEW count
        }
    mkItem quote =
      OnSearch.Item
        { id = quote.id.getId,
          vehicle_variant = show quote.vehicleVariant,
          estimated_price = OnSearch.Price $ realToFrac quote.estimatedFare,
          discount = OnSearch.Price . realToFrac <$> quote.discount,
          discounted_price = OnSearch.Price $ realToFrac quote.estimatedTotalFare,
          nearest_driver_distance = quote.distanceToNearestDriver
        }
