module Domain.Action.Beckn.Search.OneWay where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Data.Text as T
import Data.Traversable
import qualified Domain.Types.FareProduct as SFP
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
  m [DQuote.Quote]
onSearchCallback searchRequest transporterId now fromLocation toLocation = do
  pool <- DrPool.calculateDriverPool fromLocation.id transporterId Nothing SFP.ONE_WAY
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
      buildOneWayQuote searchRequest fareParams transporterId (getDistanceInMeter distance) poolResult.distanceToDriver poolResult.variant now

  Esq.runTransaction $
    for_ listOfQuotes QQuote.create
  pure listOfQuotes

buildOneWayQuote ::
  EsqDBFlow m r =>
  DSearchRequest.SearchRequest ->
  Fare.FareParameters ->
  Id DOrg.Organization ->
  Double ->
  Double ->
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
