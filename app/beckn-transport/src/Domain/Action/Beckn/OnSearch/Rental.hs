module Domain.Action.Beckn.OnSearch.Rental where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Data.Traversable
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, state)
import qualified Product.Location as Loc
import qualified Storage.Queries.Products as QProduct
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RentalFarePolicy as QRentalFarePolicy
import Tools.Metrics (CoreMetrics, HasBPPMetrics)
import Types.Error
import Utils.Common

data QuoteInfo = QuoteInfo
  { quoteId :: Id DQuote.Quote,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    baseDistance :: Kilometers,
    baseDuration :: Hours,
    descriptions :: [Text],
    fromLocation :: LatLong,
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
  DLoc.SearchReqLocation ->
  UTCTime ->
  m [QuoteInfo]
onSearchCallback searchRequest transporterId fromLocation now = do
  rentalFarePolicies <- QRentalFarePolicy.findRentalFarePoliciesByOrg transporterId

  let fromLoc = Loc.locationToLatLong fromLocation
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
      estimatedTotalFare = baseFare
  -- FIXME this request is duplicating
  products <-
    QProduct.findByName (show vehicleVariant)
      >>= fromMaybeM ProductsNotFound
  pure $
    DQuote.Quote
      { id = quoteId,
        requestId = searchRequestId,
        productId = products.id,
        providerId = organizationId,
        createdAt = now,
        quoteDetails = DQuote.mkRentalQuoteDetails rentalFarePolicy,
        ..
      }

mkQuoteInfo :: DQuote.Quote -> DRentalFP.RentalFarePolicy -> LatLong -> UTCTime -> QuoteInfo
mkQuoteInfo quote rentalFarePolicy@DRentalFP.RentalFarePolicy {..} fromLocation startTime = do
  QuoteInfo
    { quoteId = quote.id,
      vehicleVariant = quote.vehicleVariant,
      estimatedFare = quote.estimatedFare,
      discount = quote.discount,
      estimatedTotalFare = quote.estimatedTotalFare,
      descriptions = mkDescriptions rentalFarePolicy,
      ..
    }

mkDescriptions :: DRentalFP.RentalFarePolicy -> [Text]
mkDescriptions rentalFarePolicy =
  [ "Extra km fare: " <> show rentalFarePolicy.extraKmFare,
    "Extra min fare: " <> show rentalFarePolicy.extraMinuteFare,
    "Extra fare for day: " <> (maybe "not allowed" show rentalFarePolicy.driverAllowanceForDay),
    "A rider can choose this package for a trip where the rider may not have a pre-decided destination and may not want to return to the origin location",
    "The rider may want to stop at multiple destinations and have the taxi wait for the rider at these locations"
  ]
