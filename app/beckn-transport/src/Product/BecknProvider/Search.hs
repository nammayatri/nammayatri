module Product.BecknProvider.Search (search) where

import App.Types
import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Product.Validation.Context
import Beckn.Serviceability
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.Search as Search
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.List as List
import qualified Data.Text as T
import Data.Traversable
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Quote as Quote
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.SearchReqLocation as Location
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.Vehicle as Veh
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (state)
import qualified ExternalAPI.Flow as ExternalAPI
import Product.FareCalculator
import qualified Product.FareCalculator.Flow as Fare
import Product.Location
import qualified Product.Location as Loc
import qualified SharedLogic.DriverPool as DrPool
import Storage.Queries.Geometry
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Products as SProduct
import qualified Storage.Queries.Quote as Quote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchReqLocation as Loc
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Metrics (CoreMetrics, HasBPPMetrics)
import qualified Tools.Metrics as Metrics
import Types.Error
import Utils.Common

search ::
  Id Org.Organization ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReq ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult _ subscriber) (SignatureAuthResult _ gateway) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    let context = req.context
    validateContext context
    transporter <-
      Org.findById transporterId
        >>= fromMaybeM OrgDoesNotExist
    let callbackUrl = gateway.subscriber_url
    let intent = req.message.intent
    let pickup = intent.fulfillment.start
    let dropOff = intent.fulfillment.end

    let pickupLatLong = locationToLatLong pickup.location.gps
    let dropoffLatLong = locationToLatLong dropOff.location.gps

    ExternalAPI.withCallback' withRetry transporter SEARCH OnSearch.onSearchAPI context callbackUrl $ do
      unless transporter.enabled $
        throwError AgencyDisabled

      unlessM (rideServiceable someGeometriesContain pickupLatLong dropoffLatLong) $
        throwError RideNotServiceable

      whenJustM
        (QSearchRequest.findByTxnIdAndBapIdAndBppId context.transaction_id context.bap_id transporterId)
        (\_ -> throwError $ InvalidRequest "Duplicate Search request")

      searchMetricsMVar <- Metrics.startSearchMetrics transporterId

      now <- getCurrentTime
      let startTime = pickup.time.timestamp
      validity <- getValidTime now startTime
      fromLocation <- buildStartSearchReqLoc pickup.location now
      toLocation <- buildStartSearchReqLoc dropOff.location now
      let bapOrgId = subscriber.subscriber_id
      uuid <- L.generateGUID
      let bapUri = subscriber.subscriber_url
      let searchRequest = mkSearchRequest req uuid now validity startTime fromLocation toLocation transporterId bapOrgId bapUri
      Esq.runTransaction $ do
        Loc.create fromLocation
        Loc.create toLocation
        QSearchRequest.create searchRequest

      onSearchCallback searchRequest transporter fromLocation toLocation searchMetricsMVar

buildStartSearchReqLoc :: MonadFlow m => Search.Location -> UTCTime -> m Location.SearchReqLocation
buildStartSearchReqLoc loc now = do
  let Search.Gps {..} = loc.gps
      Search.Address {..} = loc.address
  locId <- generateGUID
  return
    Location.SearchReqLocation
      { id = locId,
        areaCode = area_code,
        createdAt = now,
        updatedAt = now,
        ..
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
  Text ->
  BaseUrl ->
  SearchRequest.SearchRequest
mkSearchRequest req uuid now validity startTime fromLocation toLocation transporterId bapOrgId bapUri = do
  SearchRequest.SearchRequest
    { id = Id uuid,
      transactionId = req.context.transaction_id,
      startTime = startTime,
      validTill = validity,
      providerId = transporterId,
      fromLocationId = fromLocation.id,
      toLocationId = toLocation.id,
      bapId = bapOrgId,
      bapUri = bapUri,
      createdAt = now
    }

onSearchCallback ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    HasGoogleMaps m r c,
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
  pool <- DrPool.calculateDriverPool fromLocation.id transporterId Nothing
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
    (.info.distance) <$> MapSearch.getDistance (Just MapSearch.CAR) (Loc.locationToLatLong fromLocation) (Loc.locationToLatLong toLocation)

  listOfQuotes <-
    for listOfProtoQuotes $ \poolResult -> do
      fareParams <- calculateFare transporterId poolResult.variant distance searchRequest.startTime
      buildQuote searchRequest fareParams transporterId (getDistanceInMeter distance) poolResult.distanceToDriver poolResult.variant

  Esq.runTransaction $
    for_ listOfQuotes Quote.create

  mkOnSearchMessage listOfQuotes transporter
    <* Metrics.finishSearchMetrics transporterId searchMetricsMVar

buildQuote ::
  EsqDBFlow m r =>
  SearchRequest.SearchRequest ->
  Fare.FareParameters ->
  Id Org.Organization ->
  Double ->
  Double ->
  Veh.Variant ->
  m Quote.Quote
buildQuote productSearchRequest fareParams transporterId distance nearestDriverDist vehicleVariant = do
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
  EsqDBFlow m r =>
  [Quote.Quote] ->
  Org.Organization ->
  m OnSearch.OnSearchMessage
mkOnSearchMessage quotes transporterOrg = do
  provider <- buildProvider transporterOrg quotes
  return . OnSearch.OnSearchMessage $ OnSearch.Catalog [provider]

buildProvider :: EsqDBFlow m r => Org.Organization -> [Quote.Quote] -> m OnSearch.Provider
buildProvider org quotes = do
  count <- QRide.getCountByStatus (org.id)
  let items = map mkItem quotes
  return $
    OnSearch.Provider
      { id = getShortId org.shortId,
        name = org.name,
        items,
        contacts = fromMaybe "" org.mobileNumber,
        rides_inprogress = fromMaybe 0 $ List.lookup Ride.INPROGRESS count,
        rides_completed = fromMaybe 0 $ List.lookup Ride.COMPLETED count,
        rides_confirmed = fromMaybe 0 $ List.lookup Ride.NEW count
      }
  where
    mkItem quote =
      OnSearch.Item
        { id = quote.id.getId,
          vehicle_variant = show quote.vehicleVariant,
          estimated_price = OnSearch.Price $ realToFrac quote.estimatedFare,
          discount = OnSearch.Price . realToFrac <$> quote.discount,
          discounted_price = OnSearch.Price $ realToFrac quote.estimatedTotalFare,
          nearest_driver_distance = realToFrac quote.distanceToNearestDriver
        }
