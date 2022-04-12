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
import qualified Domain.Types.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.OneWayQuote as OneWayQuote
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
import qualified Storage.Queries.FarePolicy.FareProduct as QFareProduct
import Storage.Queries.Geometry
import qualified Storage.Queries.OneWayQuote as OneWayQuote
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Products as SProduct
import qualified Storage.Queries.Quote as Quote
import qualified Storage.Queries.RentalFarePolicy as QRentalFarePolicy
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
    let callbackUrl = gateway.subscriber_url
    let intent = req.message.intent
    let pickup = intent.fulfillment.start
    let mbDropOff = intent.fulfillment.end

    transporter <-
      Org.findById transporterId
        >>= fromMaybeM OrgDoesNotExist
    let pickupLatLong = locationToLatLong pickup.location.gps
    let mbDropoffLatLong = locationToLatLong <$> (mbDropOff <&> (.location) <&> (.gps))

    ExternalAPI.withCallback' withRetry transporter SEARCH OnSearch.onSearchAPI context callbackUrl $ do
      unless transporter.enabled $
        throwError AgencyDisabled

      unlessM (rideServiceable someGeometriesContain pickupLatLong mbDropoffLatLong) $
        throwError RideNotServiceable

      whenJustM
        (QSearchRequest.findByTxnIdAndBapIdAndBppId context.transaction_id context.bap_id transporterId)
        (\_ -> throwError $ InvalidRequest "Duplicate Search request")

      searchMetricsMVar <- Metrics.startSearchMetrics transporterId

      now <- getCurrentTime
      let startTime = pickup.time.timestamp
      validity <- getValidTime now startTime
      fromLocation <- buildStartSearchReqLoc now pickup.location
      mbToLocation <- buildStartSearchReqLoc now `traverse` (mbDropOff <&> (.location))
      let bapOrgId = subscriber.subscriber_id
      uuid <- L.generateGUID
      let bapUri = subscriber.subscriber_url
      let searchRequest = mkSearchRequest req uuid now validity startTime fromLocation mbToLocation transporterId bapOrgId bapUri
      Esq.runTransaction $ do
        Loc.create fromLocation
        whenJust mbToLocation Loc.create
        QSearchRequest.create searchRequest

      fareProducts <- QFareProduct.findEnabledByOrgId transporterId
      let isRentalProduct = any (\fareProduct -> fareProduct._type == DFareProduct.RENTAL) fareProducts
      let isOneWayProduct = any (\fareProduct -> fareProduct._type == DFareProduct.ONE_WAY) fareProducts
      rentalQuotes <-
        if isRentalProduct
          then rentalOnSearchCallback searchRequest transporter.id now
          else pure []
      oneWayQuotes <-
        maybe
          (pure [])
          (\toLocation -> if isOneWayProduct then onSearchCallback searchRequest transporter now fromLocation toLocation else pure [])
          mbToLocation

      mkOnSearchMessage oneWayQuotes rentalQuotes transporter
        <* Metrics.finishSearchMetrics transporterId searchMetricsMVar

buildStartSearchReqLoc :: MonadFlow m => UTCTime -> Search.Location -> m Location.SearchReqLocation
buildStartSearchReqLoc now loc = do
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
  Maybe Location.SearchReqLocation ->
  Id Org.Organization ->
  Text ->
  BaseUrl ->
  SearchRequest.SearchRequest
mkSearchRequest req uuid now validity startTime fromLocation mbToLocation transporterId bapOrgId bapUri = do
  SearchRequest.SearchRequest
    { id = Id uuid,
      transactionId = req.context.transaction_id,
      startTime = startTime,
      validTill = validity,
      providerId = transporterId,
      fromLocationId = fromLocation.id,
      toLocationId = mbToLocation <&> (.id),
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
  UTCTime ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  m [(Quote.Quote, OneWayQuote.OneWayQuote)]
onSearchCallback searchRequest transporter now fromLocation toLocation = do
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
      buildQuote searchRequest fareParams transporterId (getDistanceInMeter distance) poolResult.distanceToDriver poolResult.variant now

  Esq.runTransaction $
    for_ listOfQuotes $ \(quote, oneWayQuote) -> do
      Quote.create quote
      OneWayQuote.create oneWayQuote

  pure listOfQuotes

buildQuote ::
  EsqDBFlow m r =>
  SearchRequest.SearchRequest ->
  Fare.FareParameters ->
  Id Org.Organization ->
  Double ->
  Double ->
  Veh.Variant ->
  UTCTime ->
  m (Quote.Quote, OneWayQuote.OneWayQuote)
buildQuote productSearchRequest fareParams transporterId distance nearestDriverDist vehicleVariant now = do
  quoteId <- Id <$> L.generateGUID
  oneWayQuoteId <- Id <$> L.generateGUID
  let estimatedFare = fareSum fareParams
      discount = fareParams.discount
      estimatedTotalFare = fareSumWithDiscount fareParams
  products <-
    SProduct.findByName (show vehicleVariant)
      >>= fromMaybeM ProductsNotFound
  let quote =
        Quote.Quote
          { id = quoteId,
            requestId = productSearchRequest.id,
            productId = products.id,
            providerId = transporterId,
            createdAt = now,
            ..
          }
  let oneWayQuote =
        OneWayQuote.OneWayQuote
          { id = oneWayQuoteId,
            quoteId = quoteId,
            distanceToNearestDriver = nearestDriverDist,
            ..
          }
  pure (quote, oneWayQuote)

mkOnSearchMessage ::
  EsqDBFlow m r =>
  [(Quote.Quote, OneWayQuote.OneWayQuote)] ->
  [Quote.Quote] ->
  Org.Organization ->
  m OnSearch.OnSearchMessage
mkOnSearchMessage oneWayQuotes rentalQuotes transporterOrg = do
  provider <- buildProvider transporterOrg oneWayQuotes rentalQuotes
  return . OnSearch.OnSearchMessage $ OnSearch.Catalog [provider]

buildProvider ::
  EsqDBFlow m r =>
  Org.Organization ->
  [(Quote.Quote, OneWayQuote.OneWayQuote)] ->
  [Quote.Quote] ->
  m OnSearch.Provider
buildProvider org oneWayQuotes rentalQuotes = do
  count <- QRide.getCountByStatus (org.id)
  let oneWayItems = map mkOneWayItem oneWayQuotes
  rentalsItems <- mapM buildRentalItem rentalQuotes
  return $
    OnSearch.Provider
      { id = getShortId org.shortId,
        name = org.name,
        items = (OnSearch.OneWay <$> oneWayItems) <> (OnSearch.Rental <$> rentalsItems),
        contacts = fromMaybe "" org.mobileNumber,
        rides_inprogress = fromMaybe 0 $ List.lookup Ride.INPROGRESS count,
        rides_completed = fromMaybe 0 $ List.lookup Ride.COMPLETED count,
        rides_confirmed = fromMaybe 0 $ List.lookup Ride.NEW count
      }
  where
    mkOneWayItem (quote, oneWayQuote) =
      OnSearch.OneWayItem
        { id = quote.id.getId,
          category_id = show DFareProduct.ONE_WAY,
          vehicle_variant = show quote.vehicleVariant,
          estimated_price = OnSearch.Price $ realToFrac quote.estimatedFare,
          discount = OnSearch.Price . realToFrac <$> quote.discount,
          discounted_price = OnSearch.Price $ realToFrac quote.estimatedTotalFare,
          nearest_driver_distance = realToFrac oneWayQuote.distanceToNearestDriver
        }
    buildRentalItem quote = do
      rentalFarePolicy <-
        QRentalFarePolicy.findRentalFarePolicyByOrgAndVehicleVariant org.id quote.vehicleVariant
          >>= fromMaybeM NoFarePolicy --NoRentalFarePolicy
      pure
        OnSearch.RentalItem
          { id = quote.id.getId,
            category_id = show DFareProduct.RENTAL,
            vehicle_variant = show quote.vehicleVariant,
            estimated_price = OnSearch.Price $ realToFrac quote.estimatedFare,
            discount = OnSearch.Price . realToFrac <$> quote.discount,
            discounted_price = OnSearch.Price $ realToFrac quote.estimatedTotalFare,
            baseDistance = rentalFarePolicy.baseDistance,
            baseDurationHr = rentalFarePolicy.baseDurationHr,
            extraKMFare = OnSearch.Price $ realToFrac rentalFarePolicy.extraKMFare,
            extraMinuteFare = OnSearch.Price $ realToFrac rentalFarePolicy.extraMinuteFare,
            driverAllowanceForDay = OnSearch.Price . realToFrac <$> rentalFarePolicy.driverAllowanceForDay
          }

rentalOnSearchCallback ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    HasGoogleMaps m r c,
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  SearchRequest.SearchRequest ->
  Id Org.Organization ->
  UTCTime ->
  m [Quote.Quote]
rentalOnSearchCallback searchRequest transporterId now = do
  listOfQuotes <- forM everyPossibleVariant $ \var -> do
    rentalFareParams <- calculateRentalFare transporterId var
    buildRentalQuote searchRequest rentalFareParams transporterId var now

  Esq.runTransaction $
    for_ listOfQuotes Quote.create

  pure listOfQuotes

buildRentalQuote ::
  EsqDBFlow m r =>
  SearchRequest.SearchRequest ->
  Fare.RentalFareParameters ->
  Id Org.Organization ->
  Veh.Variant ->
  UTCTime ->
  m Quote.Quote
buildRentalQuote productSearchRequest rentalFareParams transporterId vehicleVariant now = do
  quoteId <- Id <$> L.generateGUID
  let estimatedFare = rentalFareSum rentalFareParams
      discount = rentalFareParams.discount
      estimatedTotalFare = rentalFareSumWithDiscount rentalFareParams
  products <-
    SProduct.findByName (show vehicleVariant)
      >>= fromMaybeM ProductsNotFound
  pure
    Quote.Quote
      { id = quoteId,
        requestId = productSearchRequest.id,
        productId = products.id,
        providerId = transporterId,
        createdAt = now,
        ..
      }
