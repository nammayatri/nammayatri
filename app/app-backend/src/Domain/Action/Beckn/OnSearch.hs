module Domain.Action.Beckn.OnSearch where

import App.Types
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id, state)
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchReq
import qualified Tools.Metrics as Metrics
import Types.Error
import Utils.Common

data DOnSearchReq = DOnSearchReq
  { requestId :: Id DSearchReq.SearchRequest,
    providerInfo :: ProviderInfo,
    estimatesInfo :: [EstimateInfo],
    quotesInfo :: [QuoteInfo]
  }

data ProviderInfo = ProviderInfo
  { providerId :: Text,
    name :: Text,
    url :: BaseUrl,
    mobileNumber :: Text,
    ridesCompleted :: Int
  }

data EstimateInfo = EstimateInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    descriptions :: [Text]
  }

data QuoteInfo = QuoteInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    quoteDetails :: QuoteDetails,
    descriptions :: [Text]
  }

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | RentalDetails RentalQuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }

data RentalQuoteDetails = RentalQuoteDetails
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }

searchCb ::
  Text ->
  Maybe DOnSearchReq ->
  Flow ()
searchCb transactionId mbReq = do
  Metrics.finishSearchMetrics transactionId -- move it to api handler or acl?
  whenJust mbReq searchCbService

searchCbService ::
  DOnSearchReq ->
  Flow ()
searchCbService DOnSearchReq {..} = do
  _searchRequest <- QSearchReq.findById requestId >>= fromMaybeM (SearchRequestDoesNotExist requestId.getId)
  now <- getCurrentTime
  estimates <- traverse (buildEstimate requestId providerInfo now) estimatesInfo
  quotes <- traverse (buildQuote requestId providerInfo now) quotesInfo
  DB.runTransaction do
    QEstimate.createMany estimates
    QQuote.createMany quotes

buildEstimate ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  EstimateInfo ->
  m DEstimate.Estimate
buildEstimate requestId providerInfo now EstimateInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  pure
    DEstimate.Estimate
      { id = uid,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        ..
      }

buildQuote ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  QuoteInfo ->
  m DQuote.Quote
buildQuote requestId providerInfo now QuoteInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  quoteDetails' <- case quoteDetails of
    OneWayDetails oneWayDetails ->
      pure . DQuote.OneWayDetails $ mkOneWayQuoteDetails oneWayDetails
    RentalDetails rentalSlab -> do
      DQuote.RentalDetails <$> buildRentalSlab rentalSlab
  pure
    DQuote.Quote
      { id = uid,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        quoteDetails = quoteDetails',
        ..
      }

mkOneWayQuoteDetails :: OneWayQuoteDetails -> DQuote.OneWayQuoteDetails
mkOneWayQuoteDetails OneWayQuoteDetails {..} = DQuote.OneWayQuoteDetails {..}

buildRentalSlab :: MonadFlow m => RentalQuoteDetails -> m DRentalSlab.RentalSlab
buildRentalSlab RentalQuoteDetails {..} = do
  id <- generateGUID
  pure DRentalSlab.RentalSlab {..}

buildTripTerms ::
  MonadFlow m =>
  [Text] ->
  m (Maybe DTripTerms.TripTerms)
buildTripTerms [] = pure Nothing
buildTripTerms descriptions = do
  id <- generateGUID
  pure . Just $ DTripTerms.TripTerms {..}
