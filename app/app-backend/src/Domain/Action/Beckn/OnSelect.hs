module Domain.Action.Beckn.OnSelect
  ( module Domain.Action.Beckn.OnSelect,
  )
where

import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SelectedQuote as DSQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SelectedQuote as QSQuote
import Utils.Common

data DOnSelectReq = DOnSelectReq
  { quoteId :: Id DQuote.Quote,
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
    quoteDetails :: QuoteDetails,
    descriptions :: [Text]
  }

newtype QuoteDetails
  = AutoDetails AutoQuoteDetails

data AutoQuoteDetails = AutoQuoteDetails
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: Double,
    validTill :: UTCTime,
    rating :: Maybe Double,
    bppDriverQuoteId :: Id DQuote.BPPQuote
  }
  deriving (Generic, Show)

onSelect ::
  DOnSelectReq ->
  Flow ()
onSelect DOnSelectReq {..} = do
  _quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId) -- check that the quote is present
  now <- getCurrentTime
  quotes <- traverse (buildSelectedQuote quoteId providerInfo now) quotesInfo
  logPretty DEBUG "quotes" quotes
  whenM (duplicateCheckCond (quotes <&> (.bppQuoteId)) providerInfo.providerId) $
    throwError $ InvalidRequest "Duplicate OnSelect quote"
  DB.runTransaction $ QSQuote.createMany quotes
  where
    duplicateCheckCond :: EsqDBFlow m r => [Id DQuote.BPPQuote] -> Text -> m Bool
    duplicateCheckCond [] _ = return False
    duplicateCheckCond (bppQuoteId_ : _) bppId_ =
      isJust <$> QSQuote.findByBppIdAndQuoteId bppId_ bppQuoteId_

buildSelectedQuote ::
  MonadFlow m =>
  Id DQuote.Quote ->
  ProviderInfo ->
  UTCTime ->
  QuoteInfo ->
  m DSQuote.SelectedQuote
buildSelectedQuote quoteId providerInfo now QuoteInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  let AutoDetails autoDetails@AutoQuoteDetails {..} = quoteDetails
  pure
    DSQuote.SelectedQuote
      { id = uid,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        bppQuoteId = autoDetails.bppDriverQuoteId,
        ..
      }

buildTripTerms ::
  MonadFlow m =>
  [Text] ->
  m (Maybe DTripTerms.TripTerms)
buildTripTerms [] = pure Nothing
buildTripTerms descriptions = do
  id <- generateGUID
  pure . Just $ DTripTerms.TripTerms {..}
