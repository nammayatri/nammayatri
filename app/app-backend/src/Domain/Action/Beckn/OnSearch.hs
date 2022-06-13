module Domain.Action.Beckn.OnSearch where

import App.Types
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id, state)
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSearchReq
import qualified Tools.Metrics as Metrics
import Types.Error
import Utils.Common

data DOnSearchReq = DOnSearchReq
  { requestId :: Id DSearchReq.SearchRequest,
    providerInfo :: ProviderInfo,
    quotesInfo :: [QuoteInfo]
  }

data ProviderInfo = ProviderInfo
  { providerId :: Text,
    name :: Text,
    url :: BaseUrl,
    mobileNumber :: Text,
    ridesCompleted :: Int
  }

data QuoteInfo = QuoteInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    quoteDetails :: DQuote.QuoteAPIDetails,
    descriptions :: [Text]
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
  quotes <- traverse (buildQuote requestId providerInfo now) quotesInfo
  DB.runTransaction $ traverse_ QQuote.create quotes

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
    DQuote.OneWayAPIDetails oneWayDetails ->
      pure . DQuote.OneWayDetails $ mkOneWayQuoteDetails oneWayDetails
    DQuote.RentalAPIDetails rentalDetails -> do
      DQuote.RentalDetails <$> buildRentalQuoteDetails rentalDetails
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

mkOneWayQuoteDetails :: DQuote.OneWayQuoteAPIDetails -> DQuote.OneWayQuoteDetails
mkOneWayQuoteDetails DQuote.OneWayQuoteAPIDetails {..} = DQuote.OneWayQuoteDetails {..}

buildRentalQuoteDetails :: MonadFlow m => DQuote.RentalQuoteAPIDetails -> m DRentalSlab.RentalSlab
buildRentalQuoteDetails DQuote.RentalQuoteAPIDetails {..} = do
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
