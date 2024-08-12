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
import Domain.Types.FarePolicy.FareProductType
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.VehicleServiceTier as DVST
import Domain.Types.VehicleVariant
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Confirm as SConfirm
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import qualified Tools.Metrics as Metrics
import qualified Tools.Notifications as Notify

data DOnSelectReq = DOnSelectReq
  { bppEstimateId :: Id DEstimate.BPPEstimate,
    providerInfo :: ProviderInfo,
    quotesInfo :: [QuoteInfo]
  }

data ProviderInfo = ProviderInfo
  { providerId :: Text,
    name :: Maybe Text,
    url :: BaseUrl,
    mobileNumber :: Maybe Text
  }

data QuoteInfo = QuoteInfo
  { vehicleVariant :: VehicleVariant,
    vehicleModel :: Maybe Text,
    estimatedFare :: Price,
    discount :: Maybe Price,
    -- estimatedTotalFare :: Price,
    quoteDetails :: QuoteDetails,
    specialLocationTag :: Maybe Text,
    serviceTierName :: Maybe Text,
    serviceTierType :: Maybe DVST.VehicleServiceTierType,
    serviceTierShortDesc :: Maybe Text,
    isCustomerPrefferedSearchRoute :: Maybe Bool,
    isBlockedRoute :: Maybe Bool,
    quoteValidTill :: UTCTime
  }

data QuoteDetails
  = OneWay DriverOfferQuoteDetails
  | Ambulance DriverOfferQuoteDetails

data DriverOfferQuoteDetails = DriverOfferQuoteDetails
  { driverName :: Text,
    gender :: Maybe DPerson.Gender,
    durationToPickup :: Maybe Int, -- Seconds?
    distanceToPickup :: Maybe HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Centesimal,
    fareProductType :: Maybe FareProductType,
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
  forM_ quotes $ \quote -> do
    triggerQuoteEvent QuoteEventData {quote = quote, person = person, merchantId = searchRequest.merchantId}
  _ <- QQuote.createMany quotes
  void $ QEstimate.updateStatus DEstimate.GOT_DRIVER_QUOTE estimate.id
  if searchRequest.autoAssignEnabledV2 == Just True
    then do
      let lowestFareQuote = selectLowestFareQuote quotes
      case lowestFareQuote of
        Just autoAssignQuote -> do
          isLockAcquired <- SConfirm.tryInitTriggerLock autoAssignQuote.requestId
          when isLockAcquired $ do
            let dConfirmReq = SConfirm.DConfirmReq {personId = person.id, quote = autoAssignQuote, paymentMethodId = searchRequest.selectedPaymentMethodId}
            dConfirmRes <- SConfirm.confirm dConfirmReq
            becknInitReq <- ACL.buildInitReqV2 dConfirmRes
            handle (errHandler dConfirmRes.booking) $ do
              Metrics.startMetricsBap Metrics.INIT dConfirmRes.merchant.name searchRequest.id.getId dConfirmRes.booking.merchantOperatingCityId.getId
              void . withShortRetry $ CallBPP.initV2 dConfirmRes.providerUrl becknInitReq searchRequest.merchantId
        Nothing -> do
          bppDetails <- forM ((.providerId) <$> quotes) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
          Notify.notifyOnDriverOfferIncoming estimate.id quotes person bppDetails
    else do
      bppDetails <- forM ((.providerId) <$> quotes) (\bppId -> CQBPP.findBySubscriberIdAndDomain bppId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> bppId <> "and domain:-" <> show Context.MOBILITY))
      Notify.notifyOnDriverOfferIncoming estimate.id quotes person bppDetails
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
  if quote1.estimatedFare.amount < quote2.estimatedFare.amount
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
  let tripTerms = Nothing
      quoteDetails_ = case quoteDetails of
        OneWay qd -> qd
        Ambulance qd -> qd
  driverOffer <- buildDriverOffer estimate.id quoteDetails_ req
  let quote =
        DQuote.Quote
          { id = uid,
            providerId = providerInfo.providerId,
            providerUrl = providerInfo.url,
            createdAt = now,
            updatedAt = now,
            quoteDetails = case quoteDetails of
              OneWay _ -> DQuote.DriverOfferDetails driverOffer
              Ambulance _ -> DQuote.AmbulanceDetails driverOffer,
            requestId = estimate.requestId,
            itemId = estimate.itemId,
            validTill = quoteValidTill,
            estimatedTotalFare = estimatedFare,
            estimatedPickupDuration = Nothing,
            vehicleServiceTierType = fromMaybe (DVST.castVariantToServiceTier vehicleVariant) serviceTierType,
            tollChargesInfo =
              ((,) <$> (estimate.tollChargesInfo <&> (.tollCharges)) <*> (estimate.tollChargesInfo <&> (.tollNames)))
                <&> \(tollCharges', tollNames') ->
                  DQuote.TollChargesInfo
                    { tollCharges = tollCharges',
                      tollNames = tollNames'
                    },
            vehicleServiceTierAirConditioned = estimate.vehicleServiceTierAirConditioned,
            isAirConditioned = estimate.isAirConditioned,
            vehicleServiceTierSeatingCapacity = estimate.vehicleServiceTierSeatingCapacity,
            specialLocationName = estimate.specialLocationName,
            quoteBreakupList = [], -- Not Handling as Rate Card details not required after Select stage
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
        bppQuoteId = bppDriverQuoteId,
        status = DDriverOffer.ACTIVE,
        createdAt = now,
        updatedAt = now,
        distanceUnit = searchRequest.distanceUnit,
        distanceToPickup = convertHighPrecMetersToDistance searchRequest.distanceUnit <$> distanceToPickup,
        gender = gender,
        ..
      }

validateRequest :: DOnSelectReq -> Flow OnSelectValidatedReq
validateRequest DOnSelectReq {..} = do
  estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist $ "bppEstimateId-" <> bppEstimateId.getId)
  searchRequest <-
    QSR.findById estimate.requestId
      >>= fromMaybeM (SearchRequestDoesNotExist estimate.requestId.getId)
  let personId = searchRequest.riderId
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  whenM (duplicateCheckCond (quotesInfo <&> getBppDriverQuoteId) providerInfo.providerId) $
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
    getBppDriverQuoteId :: QuoteInfo -> Text
    getBppDriverQuoteId quoteInfo = case quoteInfo.quoteDetails of
      OneWay driverOffer -> driverOffer.bppDriverQuoteId
      Ambulance ambulanceOffer -> ambulanceOffer.bppDriverQuoteId
