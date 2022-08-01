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
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Quote as QQuote
import Utils.Common

data DOnSelectReq = DOnSelectReq
  { estimateId :: Id DEstimate.Estimate,
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
    quoteDetails :: DriverOfferQuoteDetails,
    descriptions :: [Text]
  }

data DriverOfferQuoteDetails = DriverOfferQuoteDetails
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Double,
    bppDriverQuoteId :: Id DQuote.BPPQuote
  }
  deriving (Generic, Show)

onSelect ::
  DOnSelectReq ->
  Flow ()
onSelect DOnSelectReq {..} = do
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (QuoteDoesNotExist estimateId.getId) -- FIXME EstimateDoesNotExist
  now <- getCurrentTime
  quotes <- traverse (buildSelectedQuote estimate providerInfo now) quotesInfo
  logPretty DEBUG "quotes" quotes
  whenM (duplicateCheckCond (quotesInfo <&> (.quoteDetails.bppDriverQuoteId)) providerInfo.providerId) $
    throwError $ InvalidRequest "Duplicate OnSelect quote"
  DB.runTransaction $ QQuote.createMany quotes
  where
    duplicateCheckCond :: EsqDBFlow m r => [Id DQuote.BPPQuote] -> Text -> m Bool
    duplicateCheckCond [] _ = return False
    duplicateCheckCond (bppQuoteId_ : _) bppId_ =
      isJust <$> QQuote.findByBppIdAndQuoteId bppId_ bppQuoteId_

buildSelectedQuote ::
  MonadFlow m =>
  DEstimate.Estimate ->
  ProviderInfo ->
  UTCTime ->
  QuoteInfo ->
  m DQuote.Quote
buildSelectedQuote estimate providerInfo now QuoteInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  driverOffer <- buildDriverOffer estimate.id quoteDetails
  let quote =
        DQuote.Quote
          { id = uid,
            providerMobileNumber = providerInfo.mobileNumber,
            providerName = providerInfo.name,
            providerCompletedRidesCount = providerInfo.ridesCompleted,
            providerId = providerInfo.providerId,
            providerUrl = providerInfo.url,
            createdAt = now,
            quoteDetails = DQuote.DriverOfferDetails driverOffer,
            requestId = estimate.requestId,
            ..
          }
  pure quote

buildDriverOffer ::
  MonadFlow m =>
  Id DEstimate.Estimate ->
  DriverOfferQuoteDetails ->
  m DDriverOffer.DriverOffer
buildDriverOffer estimateId DriverOfferQuoteDetails {..} = do
  uid <- generateGUID
  pure
    DDriverOffer.DriverOffer
      { id = uid,
        bppQuoteId = bppDriverQuoteId,
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
