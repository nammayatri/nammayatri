module Domain.Action.Beckn.OnSearch where

import App.Types
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchReq
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
    quoteDetails :: DQuote.QuoteDetails,
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
  quoteTerms <- traverse buildQuoteTerms descriptions
  pure
    DQuote.Quote
      { id = uid,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        ..
      }

buildQuoteTerms ::
  MonadFlow m =>
  Text ->
  m DQuote.QuoteTerms
buildQuoteTerms description = do
  uid <- generateGUID
  return
    DQuote.QuoteTerms
      { id = uid,
        ..
      }
