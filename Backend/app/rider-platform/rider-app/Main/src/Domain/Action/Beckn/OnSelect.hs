{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnSelect
  ( module Domain.Action.Beckn.OnSelect,
  )
where

import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import qualified Tools.Notifications as Notify

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
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    quoteDetails :: DriverOfferQuoteDetails,
    descriptions :: [Text]
  }

data DriverOfferQuoteDetails = DriverOfferQuoteDetails
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Centesimal,
    bppDriverQuoteId :: Id DDriverOffer.BPPQuote
  }
  deriving (Generic, Show)

onSelect ::
  BaseUrl ->
  DOnSelectReq ->
  Flow ()
onSelect registryUrl DOnSelectReq {..} = do
  estimate <- QEstimate.findById estimateId (Proxy @Flow) >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  searchRequest <-
    QSR.findById estimate.requestId (Proxy @Flow)
      >>= fromMaybeM (SearchRequestDoesNotExist estimate.requestId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  let personId = searchRequest.riderId
  person <- Person.findById personId (Proxy @Flow) >>= fromMaybeM (PersonNotFound personId.getId)
  now <- getCurrentTime
  quotes <- traverse (buildSelectedQuote estimate providerInfo now) quotesInfo
  logPretty DEBUG "quotes" quotes
  whenM (duplicateCheckCond (quotesInfo <&> (.quoteDetails.bppDriverQuoteId)) providerInfo.providerId) $
    throwError $ InvalidRequest "Duplicate OnSelect quote"
  DB.runTransaction $ do
    QQuote.createMany @Flow quotes
    QPFS.updateStatus searchRequest.riderId DPFS.DRIVER_OFFERED_QUOTE {estimateId = estimate.id, validTill = searchRequest.validTill}
    QEstimate.updateStatus estimateId $ Just DEstimate.GOT_DRIVER_QUOTE
  Notify.notifyOnDriverOfferIncoming estimateId quotes person
  where
    duplicateCheckCond :: forall m r. EsqDBFlow m r => [Id DDriverOffer.BPPQuote] -> Text -> m Bool
    duplicateCheckCond [] _ = return False
    duplicateCheckCond (bppQuoteId_ : _) bppId_ =
      isJust <$> QQuote.findByBppIdAndBPPQuoteId bppId_ bppQuoteId_ (Proxy @m)

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
