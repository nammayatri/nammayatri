module Domain.Action.Beckn.OnSearch.Rental where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Traversable
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalFarePolicy as DRentalFP
import qualified Domain.Types.SearchRequest as DSearchRequest
import EulerHS.Prelude hiding (id, state)
import qualified Storage.Queries.Products as QProduct
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RentalFarePolicy as QRentalFarePolicy
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
  Id DSearchRequest.SearchRequest ->
  Id DOrg.Organization ->
  UTCTime ->
  m [DQuote.Quote]
onSearchCallback searchRequestId transporterId now = do
  rentalFarePolicies <- QRentalFarePolicy.findRentalFarePoliciesByOrg transporterId

  listOfQuotes <- forM rentalFarePolicies $ buildRentalQuote searchRequestId now

  Esq.runTransaction $
    for_ listOfQuotes QQuote.create

  pure listOfQuotes

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
