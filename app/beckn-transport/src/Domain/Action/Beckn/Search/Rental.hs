module Domain.Action.Beckn.Search.Rental where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Traversable
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalFarePolicy as DRentalFarePolicy
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, state)
import qualified Storage.Queries.Products as QProduct
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RentalFarePolicy as QRentalFarePolicy
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
    baseDistance :: Double,
    baseDurationHr :: Int,
    descriptions :: [Text]
  }

onSearchCallback ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    HasGoogleMaps m r c,
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  Id DSearchRequest.SearchRequest ->
  Id DOrg.Organization ->
  UTCTime ->
  m [QuoteInfo]
onSearchCallback searchRequestId transporterId now = do
  rentalFarePolicies <- QRentalFarePolicy.findRentalFarePoliciesByOrg transporterId

  listOfTuples <- forM rentalFarePolicies $ \rentalFarePolicy -> do
    quote <- buildQuote searchRequestId now rentalFarePolicy
    let quoteInfo = mkQuoteInfo quote rentalFarePolicy
    pure (quote, quoteInfo)
  let (listOfQuotes, quotesInfo) = unzip listOfTuples

  Esq.runTransaction $
    for_ listOfQuotes QQuote.createRentalQuote

  pure quotesInfo

buildQuote ::
  EsqDBFlow m r =>
  Id DSearchRequest.SearchRequest ->
  UTCTime ->
  DRentalFarePolicy.RentalFarePolicy ->
  m DQuote.RentalQuote
buildQuote searchRequestId now DRentalFarePolicy.RentalFarePolicy {..} = do
  quoteId <- Id <$> generateGUID
  let estimatedFare = baseFare
      discount = Nothing -- FIXME we don't have discount in RentalFarePolicy now
      estimatedTotalFare = baseFare
  -- FIXME this request is duplicating
  products <-
    QProduct.findByName (show vehicleVariant)
      >>= fromMaybeM ProductsNotFound
  pure $
    DQuote.RentalQuote
      { id = quoteId,
        requestId = searchRequestId,
        productId = products.id,
        providerId = organizationId,
        createdAt = now,
        ..
      }

mkQuoteInfo :: DQuote.RentalQuote -> DRentalFarePolicy.RentalFarePolicy -> QuoteInfo
mkQuoteInfo quote rentalFarePolicy@DRentalFarePolicy.RentalFarePolicy {..} = do
  QuoteInfo
    { quoteId = quote.id,
      fareProductType = DFareProduct.RENTAL,
      vehicleVariant = quote.vehicleVariant,
      estimatedFare = quote.estimatedFare,
      discount = quote.discount,
      estimatedTotalFare = quote.estimatedTotalFare,
      descriptions = mkDescriptions rentalFarePolicy,
      ..
    }

mkDescriptions :: DRentalFarePolicy.RentalFarePolicy -> [Text]
mkDescriptions rentalFarePolicy =
  [ "Extra km fare: " <> show rentalFarePolicy.extraKmFare,
    "Extra min fare: " <> show rentalFarePolicy.extraMinuteFare,
    "Extra fare for day: " <> (maybe "not allowed" show rentalFarePolicy.driverAllowanceForDay),
    "A rider can choose this package for a trip where the rider may not have a pre-decided destination and may not want to return to the origin location",
    "The rider may want to stop at multiple destinations and have the taxi wait for the rider at these locations"
  ]
