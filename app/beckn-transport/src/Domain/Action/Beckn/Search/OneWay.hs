module Domain.Action.Beckn.Search.OneWay where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Data.Text as T
import Data.Traversable
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, state)
import Product.FareCalculator
import qualified Product.FareCalculator.Flow as Fare
import qualified Product.Location as Loc
import qualified SharedLogic.DriverPool as DrPool
import qualified Storage.Queries.Products as QProduct
import qualified Storage.Queries.Quote as QQuote
import Tools.Metrics (CoreMetrics, HasBPPMetrics)
import Types.Error
import Utils.Common

data QuoteInfo = QuoteInfo
  { quoteId :: Id DQuote.Quote,
    fareProductType :: DFareProduct.FareProductType,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    distanceToNearestDriver :: Double
  }

onSearchCallback ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    HasGoogleMaps m r c,
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

  listOfQuotesAndDistances <-
    for listOfProtoQuotes $ \poolResult -> do
      fareParams <- calculateFare transporterId poolResult.variant distance searchRequest.startTime
      quote <- buildQuote searchRequest fareParams transporterId (getDistanceInMeter distance) poolResult.distanceToDriver poolResult.variant now
      pure (quote, poolResult.distanceToDriver)

  Esq.runTransaction $
    for_ (map fst listOfQuotesAndDistances) QQuote.create
  pure $ uncurry mkQuoteInfo <$> listOfQuotesAndDistances

buildQuote ::
  EsqDBFlow m r =>
  DSearchRequest.SearchRequest ->
  Fare.FareParameters ->
  Id DOrg.Organization ->
  Double ->
  Double ->
  DVeh.Variant ->
  UTCTime ->
  m DQuote.Quote
buildQuote productSearchRequest fareParams transporterId distance distanceToNearestDriver vehicleVariant now = do
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

mkQuoteInfo :: DQuote.Quote -> Double -> QuoteInfo
mkQuoteInfo DQuote.Quote {..} distanceToNearestDriver =
  QuoteInfo
    { quoteId = id,
      fareProductType = DFareProduct.ONE_WAY,
      ..
    }
