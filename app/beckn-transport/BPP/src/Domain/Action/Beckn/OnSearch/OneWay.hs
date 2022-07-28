module Domain.Action.Beckn.OnSearch.OneWay where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (..))
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common
import qualified Data.Text as T
import Data.Traversable
import qualified Domain.Types.BusinessEvent as SB
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, state)
import qualified SharedLogic.DriverPool as DrPool
import SharedLogic.FareCalculator.OneWayFareCalculator
import qualified SharedLogic.FareCalculator.OneWayFareCalculator.Flow as Fare
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Products as QProduct
import qualified Storage.Queries.Quote as QQuote
import Tools.Error
import Tools.Metrics (CoreMetrics, HasBPPMetrics)

data QuoteInfo = QuoteInfo
  { quoteId :: Id DQuote.Quote,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    distanceToNearestDriver :: HighPrecMeters,
    fromLocation :: MapSearch.LatLong,
    toLocation :: MapSearch.LatLong,
    startTime :: UTCTime
  }

onSearchCallback ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    HasGoogleMaps m r,
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  DSearchRequest.SearchRequest ->
  Id DOrg.Organization ->
  UTCTime ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  m [QuoteInfo]
onSearchCallback searchRequest transporterId now fromLocation toLocation = do
  let fromLoc = getCoordinates fromLocation
  pool <- DrPool.calculateDriverPool fromLoc transporterId Nothing SFP.ONE_WAY

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

  distance <-
    metersToHighPrecMeters . (.distance) <$> MapSearch.getDistance (Just MapSearch.CAR) fromLocation toLocation
  listOfQuotes <-
    for listOfProtoQuotes $ \poolResult -> do
      fareParams <- calculateFare transporterId poolResult.variant distance searchRequest.startTime
      buildOneWayQuote
        searchRequest
        fareParams
        transporterId
        distance
        (metersToHighPrecMeters poolResult.distanceToPickup)
        poolResult.variant now
  Esq.runTransaction $
    for_ listOfQuotes QQuote.create
  pure $ mkQuoteInfo fromLocation toLocation now distance <$> listOfQuotes

buildOneWayQuote ::
  EsqDBFlow m r =>
  DSearchRequest.SearchRequest ->
  Fare.OneWayFareParameters ->
  Id DOrg.Organization ->
  HighPrecMeters ->
  HighPrecMeters ->
  DVeh.Variant ->
  UTCTime ->
  m DQuote.Quote
buildOneWayQuote productSearchRequest fareParams transporterId distance distanceToNearestDriver vehicleVariant now = do
  quoteId <- Id <$> generateGUID
  let estimatedFare = fareSum fareParams
      discount = fareParams.discount
      estimatedTotalFare = fareSumWithDiscount fareParams
  products <- QProduct.findByName (show vehicleVariant) >>= fromMaybeM ProductsNotFound
  let oneWayQuoteDetails = DQuote.OneWayQuoteDetails {..}
  pure
    DQuote.Quote
      { id = quoteId,
        requestId = productSearchRequest.id,
        productId = products.id,
        providerId = transporterId,
        createdAt = now,
        quoteDetails = DQuote.OneWayDetails oneWayQuoteDetails,
        ..
      }

mkQuoteInfo :: DLoc.SearchReqLocation -> DLoc.SearchReqLocation -> UTCTime -> HighPrecMeters -> DQuote.Quote -> QuoteInfo
mkQuoteInfo fromLoc toLoc startTime distanceToNearestDriver DQuote.Quote {..} = do
  let fromLocation = getCoordinates fromLoc
      toLocation = getCoordinates toLoc
  QuoteInfo
    { quoteId = id,
      ..
    }
