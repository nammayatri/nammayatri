module Domain.Action.Beckn.OnSearch.Rental where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (..))
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Utils.Common
import Data.Traversable
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, state)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRentalFarePolicy
import qualified Storage.Queries.Quote as QQuote
import Tools.Metrics (CoreMetrics, HasBPPMetrics)

data QuoteInfo = QuoteInfo
  { quoteId :: Id DQuote.Quote,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    baseDistance :: Kilometers,
    baseDuration :: Hours,
    descriptions :: [Text],
    fromLocation :: LatLong,
    startTime :: UTCTime
  }

onSearchCallback ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasGoogleMaps m r,
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  DSearchRequest.SearchRequest ->
  Id DOrg.Organization ->
  DLoc.SearchReqLocation ->
  UTCTime ->
  m [QuoteInfo]
onSearchCallback searchRequest transporterId fromLocation now = do
  rentalFarePolicies <- QRentalFarePolicy.findAllByOrgId transporterId

  let fromLoc = getCoordinates fromLocation
  (listOfQuotes, quoteInfos) <- fmap unzip $
    forM rentalFarePolicies $ \fp -> do
      quote <- buildRentalQuote searchRequest.id now fp
      let quoteInfo = mkQuoteInfo quote fp fromLoc searchRequest.startTime
      return (quote, quoteInfo)

  Esq.runTransaction $
    for_ listOfQuotes QQuote.create

  pure quoteInfos

buildRentalQuote ::
  EsqDBFlow m r =>
  Id DSearchRequest.SearchRequest ->
  UTCTime ->
  DRentalFP.RentalFarePolicy ->
  m DQuote.Quote
buildRentalQuote searchRequestId now rentalFarePolicy@DRentalFP.RentalFarePolicy {..} = do
  quoteId <- Id <$> generateGUID
  let estimatedFare = baseFare
      discount = Nothing -- FIXME we don't have discount in RentalFarePolicy now
      estimatedTotalFare = estimatedFare
  -- FIXME this request is duplicating
  pure $
    DQuote.Quote
      { id = quoteId,
        requestId = searchRequestId,
        providerId = organizationId,
        createdAt = now,
        quoteDetails = DQuote.RentalDetails rentalFarePolicy,
        ..
      }

mkQuoteInfo :: DQuote.Quote -> DRentalFP.RentalFarePolicy -> LatLong -> UTCTime -> QuoteInfo
mkQuoteInfo quote DRentalFP.RentalFarePolicy {..} fromLocation startTime = do
  QuoteInfo
    { quoteId = quote.id,
      vehicleVariant = quote.vehicleVariant,
      estimatedFare = quote.estimatedFare,
      discount = quote.discount,
      estimatedTotalFare = quote.estimatedTotalFare,
      ..
    }
