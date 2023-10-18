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

import qualified Beckn.ACL.Init as ACL
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Confirm as SConfirm
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify

data DOnSelectReq = DOnSelectReq
  { bppEstimateId :: Id DEstimate.BPPEstimate,
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
    descriptions :: [Text],
    specialLocationTag :: Maybe Text
  }

data DriverOfferQuoteDetails = DriverOfferQuoteDetails
  { driverName :: Text,
    driverId :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Centesimal,
    bppDriverQuoteId :: Text
  }
  deriving (Generic, Show)

data OnSelectValidatedReq = OnSelectValidatedReq
  { estimate :: DEstimate.Estimate,
    searchRequest :: DSearchRequest.SearchRequest,
    person :: DPerson.Person,
    bppEstimateId :: Id DEstimate.BPPEstimate,
    providerInfo :: ProviderInfo,
    quotesInfo :: [QuoteInfo]
  }
  deriving (Generic)

onSelect ::
  OnSelectValidatedReq ->
  Flow ()
onSelect OnSelectValidatedReq {..} = do
  now <- getCurrentTime
  quotes <- traverse (buildSelectedQuote estimate providerInfo now searchRequest) quotesInfo
  logPretty DEBUG "quotes" quotes
  forM_ quotes $ \quote -> do
    triggerQuoteEvent QuoteEventData {quote = quote, person = person, merchantId = searchRequest.merchantId}
  _ <- QQuote.createMany quotes
  _ <- QPFS.updateStatus searchRequest.riderId DPFS.DRIVER_OFFERED_QUOTE {estimateId = estimate.id, validTill = searchRequest.validTill}
  void $ QEstimate.updateStatus estimate.id DEstimate.GOT_DRIVER_QUOTE
  QPFS.clearCache searchRequest.riderId

  if searchRequest.autoAssignEnabledV2 == Just True
    then do
      let lowestFareQuote = selectLowestFareQuote quotes
      case lowestFareQuote of
        Just autoAssignQuote -> do
          let dConfirmReq = SConfirm.DConfirmReq {personId = person.id, quoteId = autoAssignQuote.id, paymentMethodId = searchRequest.selectedPaymentMethodId}
          dConfirmRes <- SConfirm.confirm dConfirmReq
          becknInitReq <- ACL.buildInitReq dConfirmRes
          handle (errHandler dConfirmRes.booking) $ void $ withShortRetry $ CallBPP.init dConfirmRes.providerUrl becknInitReq
        Nothing -> Notify.notifyOnDriverOfferIncoming estimate.id quotes person
    else do
      Notify.notifyOnDriverOfferIncoming estimate.id quotes person
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking booking
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking booking
      | otherwise = throwM exc

selectLowestFareQuote :: [DQuote.Quote] -> Maybe DQuote.Quote
selectLowestFareQuote (quoteInfo : quoteInfoArray) =
  if null quoteInfoArray
    then Just quoteInfo
    else do
      restQuoteResult <- selectLowestFareQuote quoteInfoArray
      Just $ comparator quoteInfo restQuoteResult
selectLowestFareQuote [] = Nothing

comparator :: DQuote.Quote -> DQuote.Quote -> DQuote.Quote
comparator quote1 quote2 =
  if quote1.estimatedFare < quote2.estimatedFare
    then quote1
    else quote2

buildSelectedQuote ::
  MonadFlow m =>
  DEstimate.Estimate ->
  ProviderInfo ->
  UTCTime ->
  DSearchRequest.SearchRequest ->
  QuoteInfo ->
  m DQuote.Quote
buildSelectedQuote estimate providerInfo now req@DSearchRequest.SearchRequest {..} QuoteInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  driverOffer <- buildDriverOffer estimate.id quoteDetails req
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
            itemId = estimate.itemId,
            ..
          }
  pure quote

buildDriverOffer ::
  MonadFlow m =>
  Id DEstimate.Estimate ->
  DriverOfferQuoteDetails ->
  DSearchRequest.SearchRequest ->
  m DDriverOffer.DriverOffer
buildDriverOffer estimateId DriverOfferQuoteDetails {..} searchRequest = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DDriverOffer.DriverOffer
      { id = uid,
        merchantId = Just searchRequest.merchantId,
        merchantOperatingCityId = Just searchRequest.merchantOperatingCityId,
        driverId = Just driverId,
        bppQuoteId = bppDriverQuoteId,
        status = DDriverOffer.ACTIVE,
        updatedAt = now,
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

validateRequest :: DOnSelectReq -> Flow OnSelectValidatedReq
validateRequest DOnSelectReq {..} = do
  estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist $ "bppEstimateId-" <> bppEstimateId.getId)
  searchRequest <-
    QSR.findById estimate.requestId
      >>= fromMaybeM (SearchRequestDoesNotExist estimate.requestId.getId)
  let personId = searchRequest.riderId
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  whenM (duplicateCheckCond (quotesInfo <&> (.quoteDetails.bppDriverQuoteId)) providerInfo.providerId) $
    throwError $ InvalidRequest "Duplicate OnSelect quote"
  return $
    OnSelectValidatedReq
      { ..
      }
  where
    duplicateCheckCond :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => [Text] -> Text -> m Bool
    duplicateCheckCond [] _ = return False
    duplicateCheckCond (bppQuoteId_ : _) bppId_ =
      isJust <$> runInReplica (QQuote.findByBppIdAndBPPQuoteId bppId_ bppQuoteId_)
