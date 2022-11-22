module Domain.Action.Beckn.OnSearch.OneWay where

import qualified Beckn.External.Maps.Types as MapSearch
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.Text as T
import Data.Traversable
import qualified Domain.Types.BusinessEvent as SB
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, state)
import qualified SharedLogic.CacheDistance as CD
import qualified SharedLogic.DriverPool as DrPool
import SharedLogic.FareCalculator.OneWayFareCalculator
import qualified SharedLogic.FareCalculator.OneWayFareCalculator.Flow as Fare
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Quote as QQuote
import Tools.Maps (HasCoordinates (..))
import qualified Tools.Maps as MapSearch
import Tools.Metrics (CoreMetrics, HasBPPMetrics)

data QuoteInfo = QuoteInfo
  { quoteId :: Id DQuote.Quote,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    distanceToNearestDriver :: Meters,
    fromLocation :: MapSearch.LatLong,
    toLocation :: MapSearch.LatLong,
    startTime :: UTCTime
  }

onSearchCallback ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["driverEstimatedPickupDuration" ::: Seconds],
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  DSearchRequest.SearchRequest ->
  Id DM.Merchant ->
  UTCTime ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Text ->
  m [QuoteInfo]
onSearchCallback searchRequest transporterId now fromLocation toLocation transactionId = do
  pool <- DrPool.calculateDriverPool fromLocation transporterId Nothing SFP.ONE_WAY
  logTagInfo "OnSearchCallback" $
    "Calculated Driver Pool for organization " +|| getId transporterId
      ||+ " with drivers " +| T.intercalate ", " (getId . (.driverId) <$> pool) |+ ""
  Esq.runTransaction $ traverse_ (QBE.logDriverInPoolEvent SB.ON_SEARCH Nothing) pool
  let listOfProtoQuotes =
        catMaybes $
          everyPossibleVariant <&> \var ->
            find ((== var) . (.variant)) pool
  -- drivers sorted from nearest to furthest, so with `find`
  -- we take nearest one and calculate fare and make PI for him

  driverEstimatedPickupDuration <- asks (.driverEstimatedPickupDuration)
  distRes <-
    MapSearch.getDistance transporterId $
      MapSearch.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just MapSearch.CAR
        }
  let distance = distRes.distance
      estimatedRideDuration = distRes.duration
      estimatedRideFinishTime = realToFrac (driverEstimatedPickupDuration + estimatedRideDuration) `addUTCTime` searchRequest.startTime
  CD.cacheDistance transactionId (distance, estimatedRideDuration)
  listOfQuotes <-
    for listOfProtoQuotes $ \poolResult -> do
      fareParams <- calculateFare transporterId poolResult.variant distance estimatedRideFinishTime
      buildOneWayQuote
        searchRequest
        fareParams
        transporterId
        distance
        poolResult.distanceToPickup
        poolResult.variant
        estimatedRideFinishTime
        now
  Esq.runTransaction $
    for_ listOfQuotes QQuote.create
  pure $ mkQuoteInfo fromLocation toLocation now <$> listOfQuotes

buildOneWayQuote ::
  EsqDBFlow m r =>
  DSearchRequest.SearchRequest ->
  Fare.OneWayFareParameters ->
  Id DM.Merchant ->
  Meters ->
  Meters ->
  DVeh.Variant ->
  UTCTime ->
  UTCTime ->
  m DQuote.Quote
buildOneWayQuote productSearchRequest fareParams transporterId distance distanceToNearestDriver vehicleVariant estimatedFinishTime now = do
  quoteId <- Id <$> generateGUID
  let estimatedFare = fareSum fareParams
      discount = fareParams.discount
      estimatedTotalFare = fareSumWithDiscount fareParams
  let oneWayQuoteDetails = DQuote.OneWayQuoteDetails {..}
  pure
    DQuote.Quote
      { id = quoteId,
        requestId = productSearchRequest.id,
        providerId = transporterId,
        createdAt = now,
        quoteDetails = DQuote.OneWayDetails oneWayQuoteDetails,
        ..
      }

mkQuoteInfo :: DLoc.SearchReqLocation -> DLoc.SearchReqLocation -> UTCTime -> DQuote.Quote -> QuoteInfo
mkQuoteInfo fromLoc toLoc startTime DQuote.Quote {..} = do
  let fromLocation = getCoordinates fromLoc
      toLocation = getCoordinates toLoc
      distanceToNearestDriver = case quoteDetails of
        DQuote.OneWayDetails details -> DQuote.distanceToNearestDriver details
        _ -> 0
  QuoteInfo
    { quoteId = id,
      ..
    }
