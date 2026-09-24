{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.Beckn.Select
  ( DSelectReq (..),
    validateRequest,
    handler,
    validateQuoteSelect,
    handleQuoteSelect,
  )
where

import qualified Beckn.OnDemand.Transformer.OndcScheduledRide.OnSelect as OSROnSelect
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as BUtils
import Control.Applicative ((<|>))
import Data.Either.Extra (eitherToMaybe)
import Data.Text as Text hiding (find)
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types.AddOnConfig as DAddOnConfig
import qualified Domain.Types.ConditionalCharges as DAC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.FareParameters as DFareParams
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ParcelType as DParcel
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Yudhishthira as Y
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.AppMetrics as Metrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
-- import qualified Lib.Yudhishthira.Event as Yudhishthira
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified SharedLogic.AddOn as SAddOn
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CallBAP as CallBAP
import SharedLogic.DriverPool
import qualified SharedLogic.FarePolicy as SFP
import qualified SharedLogic.MetricsLabels as SML
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.SearchTry
import qualified SharedLogic.Type as SLT
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.FareParameters as QFareParams
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import qualified Tools.Metrics.ARDUBPPMetrics as BPPMetrics

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    estimateIds :: [Id DEst.Estimate],
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupTime :: UTCTime,
    autoAssignEnabled :: Bool,
    customerExtraFee :: Maybe HighPrecMoney,
    -- | BAP-proposed total fare for the Quote-based /select negotiation flow
    -- (ONDC v2.1.0 Pre-Order Bid). Layer 1 (Beckn.ACL.Select) always sets this
    -- to Nothing; only Beckn.OnDemand.Transformer.OndcScheduledRide.Select.ondcScheduledRideParser fills
    -- it in, from item.price.value, for enableOndcScheduledRideSupport BAPs
    -- pilot merchants. Deliberately a separate field from customerExtraFee,
    -- which is an additive tip/extra-fee delta used by the Estimate-based
    -- dynamic-offer flow -- this one is the bid's absolute proposed total.
    negotiatedFare :: Maybe HighPrecMoney,
    negativeFareAdjustment :: Maybe HighPrecMoney,
    isPetRide :: Bool,
    customerPhoneNum :: Maybe Text,
    -- | Customer display name from fulfillment.customer.person (value-add-NP BAPs
    -- send it for the one-shot assignment flow); stored on SearchRequest.riderName.
    customerName :: Maybe Text,
    isAdvancedBookingEnabled :: Bool,
    isMultipleOrNoDeviceIdExist :: Maybe Bool,
    toUpdateDeviceIdInfo :: Bool,
    disabilityDisable :: Maybe Bool,
    parcelDetails :: (Maybe Text, Maybe Int),
    preferSafetyPlus :: Bool,
    driverPreference :: Maybe [Text],
    billingCategory :: SLT.BillingCategory,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    emailDomain :: Maybe Text,
    customerRating :: Maybe Centesimal,
    customerTotalRatings :: Maybe Int,
    customerGender :: Maybe DP.Gender,
    businessEmailDomain :: Maybe Text,
    -- | A BAP can select more than one add-on on the same item (e.g. rider
    -- insurance plus a future second add-on) -- empty when none was
    -- selected, never a single Maybe.
    addOns :: [Spec.AddOn]
  }

-- user can select array of estimate because of book any option, in most of the cases it will be a single estimate
handler :: DM.Merchant -> DSelectReq -> DSR.SearchRequest -> [DEst.Estimate] -> [DAddOnConfig.AddOnData] -> Flow ()
handler merchant sReq searchReq estimates addOnData = do
  logDebug $ "DSelectReq: select request billingCategory: " <> show sReq.billingCategory <> "transactionId: " <> sReq.transactionId
  whenJust (listToMaybe estimates) $ \primaryEstimate -> do
    cityLabel <- SML.getCityLabel searchReq.merchantOperatingCityId
    distanceEdges <- SML.getDistanceBucketEdges searchReq.merchantOperatingCityId
    let (pickupZone, dropZone) = SML.specialZoneLabels searchReq.area
    BPPMetrics.incrementRiderAcceptanceCount
      merchant.shortId.getShortId
      cityLabel
      (show primaryEstimate.vehicleServiceTier)
      "normal"
      (SML.distanceBucketLabel distanceEdges primaryEstimate.estimatedDistance)
      pickupZone
      dropZone
  now <- getCurrentTime
  mbRiderDetails <- case sReq.customerPhoneNum of
    Just number -> do
      let mbMerchantOperatingCityId = Just searchReq.merchantOperatingCityId
      -- consent tag is only emitted at confirm, not select, so no consent to record yet here
      (riderDetails, isNewRider) <- SRD.getRiderDetails searchReq.currency merchant.id mbMerchantOperatingCityId (fromMaybe "+91" merchant.mobileCountryCode) number searchReq.bapId False Nothing
      when isNewRider $ QRD.create riderDetails
      QRD.updateCustomerProfile sReq.customerRating sReq.customerTotalRatings sReq.customerGender riderDetails.id
      when sReq.toUpdateDeviceIdInfo do
        let mbFlag = mbGetPayoutFlag sReq.isMultipleOrNoDeviceIdExist
        when (riderDetails.payoutFlagReason /= mbFlag) $ QRD.updateFlagReasonAndIsDeviceIdExists mbFlag (Just $ isJust sReq.isMultipleOrNoDeviceIdExist) riderDetails.id
      return $ Just riderDetails {DRD.customerRating = sReq.customerRating, DRD.customerTotalRatings = sReq.customerTotalRatings, DRD.customerGender = sReq.customerGender}
    Nothing -> do
      logWarning "Failed to get rider details as BAP Phone Number is NULL"
      return Nothing
  let riderId = (.id) <$> mbRiderDetails
  when sReq.isPetRide $ do
    let tagData =
          Y.SelectTagData
            { isPetRide = sReq.isPetRide
            -- ,estimates = estimates uncomment this line if you want to use estimates in select tag data
            }
    addNammaTags tagData searchReq
  tripQuoteDetails <-
    estimates `forM` \estimate -> do
      QDQ.setInactiveAllDQByEstId estimate.id now
      let mbDriverExtraFeeBounds = ((,) <$> estimate.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> estimate.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> estimate.farePolicy)
          driverParkingCharge = join $ (.parkingCharge) <$> estimate.farePolicy
          driverAdditionalCharges = filterChargesByApplicability $ fromMaybe [] ((.conditionalCharges) <$> estimate.farePolicy)
          petCharges' = if sReq.isPetRide then (.petCharges) =<< estimate.farePolicy else Nothing
          businessDiscount = if sReq.billingCategory == SLT.BUSINESS then fromMaybe 0.0 estimate.businessDiscount else 0.0
          personalDiscount = if sReq.billingCategory == SLT.PERSONAL then fromMaybe 0.0 estimate.personalDiscount else 0.0
      buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 sReq.customerExtraFee + fromMaybe 0 sReq.negativeFareAdjustment + fromMaybe 0 petCharges' - businessDiscount - personalDiscount) Nothing (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId driverAdditionalCharges False ((.congestionCharge) =<< estimate.fareParams) petCharges' (estimate.fareParams >>= (.priorityCharges)) estimate.commissionCharges (estimate.fareParams >>= (.tollCharges)) (estimate.fareParams >>= (.govtCharges)) (estimate.fareParams >>= (.driverCancellationNotAllowed))
  let parcelType = (fst sReq.parcelDetails) >>= \rpt -> readMaybe @DParcel.ParcelType $ unpack rpt
      -- Quotes-first airport flow: the picked estimate carries its own gate area (e.g.
      -- Pickup_<slId>_Gate_<gateId>). Refine the SearchRequest.area/pickupGateId from
      -- that so per-gate driver-queue routing (Redis DriverDemand:Gate:<gateId>:<variant>)
      -- dispatches to the right pool.
      mbEstimateArea = listToMaybe estimates >>= (.area) >>= (readMaybe . Text.unpack)
      mbEstimateGateId = mbEstimateArea >>= SL.pickupGateIdFromArea
      updatedSearchRequest =
        searchReq
          { DSR.disabilityTag = if sReq.disabilityDisable == Just True then Nothing else searchReq.disabilityTag,
            DSR.isAdvanceBookingEnabled = sReq.isAdvancedBookingEnabled || searchReq.isAdvanceBookingEnabled,
            DSR.autoAssignEnabled = if sReq.autoAssignEnabled then Just sReq.autoAssignEnabled else searchReq.autoAssignEnabled,
            DSR.riderId = riderId,
            DSR.riderName = sReq.customerName <|> searchReq.riderName,
            DSR.parcelType = if isJust parcelType then parcelType else searchReq.parcelType,
            DSR.parcelQuantity = if isJust parcelType then snd sReq.parcelDetails else searchReq.parcelQuantity,
            DSR.preferSafetyPlus = sReq.preferSafetyPlus,
            DSR.isPetRide = sReq.isPetRide,
            DSR.area = mbEstimateArea <|> searchReq.area,
            DSR.pickupGateId = mbEstimateGateId <|> searchReq.pickupGateId
          }
  QSR.updateMultipleByRequestId updatedSearchRequest searchReq.isScheduled
  QSR.updateByPrimaryKey updatedSearchRequest
  let driverSearchBatchInput =
        DriverSearchBatchInput
          { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
            merchant,
            searchReq = updatedSearchRequest,
            tripQuoteDetails,
            customerExtraFee = sReq.customerExtraFee,
            negativeFareAdjustment = sReq.negativeFareAdjustment,
            messageId = sReq.messageId,
            isRepeatSearch = False,
            billingCategory = sReq.billingCategory,
            isAllocatorBatch = False,
            paymentMethodInfo = sReq.paymentMethodInfo,
            riderDetails = mbRiderDetails,
            emailDomain = sReq.emailDomain,
            businessEmailDomain = sReq.businessEmailDomain,
            driverPreference = sReq.driverPreference,
            addOnData = addOnData
          }
  void $ initiateDriverSearchBatch driverSearchBatchInput
  -- NOTE: Special zone demand pipeline has been moved to Init handler (Domain.Action.Beckn.Init)
  -- so that it fires for both estimate-based (Select → Init) and quote-based (direct Init)
  -- flows. Special zone OTP rides skip Select entirely, so demand was never incrementing.
  Metrics.finishGenericLatencyMetrics Metrics.SELECT_TO_SEND_REQUEST searchReq.transactionId
  where
    mbGetPayoutFlag isMultipleOrNoDeviceIdExist = maybe Nothing (\val -> if val then Just DRD.MultipleDeviceIdExists else Nothing) isMultipleOrNoDeviceIdExist
    filterChargesByApplicability conditionalCharges = do
      let safetyCharges = if sReq.preferSafetyPlus then find (\ac -> (ac.chargeCategory) == DAC.SAFETY_PLUS_CHARGES) conditionalCharges else Nothing
          nyregularCharges = if fromMaybe False searchReq.isReserveRide then find (\ac -> (ac.chargeCategory) == DAC.NYREGULAR_SUBSCRIPTION_CHARGE) conditionalCharges else Nothing
      catMaybes $ [safetyCharges, nyregularCharges]

validateRequest :: Id DM.Merchant -> DSelectReq -> Bool -> Flow (DM.Merchant, DSR.SearchRequest, [DEst.Estimate], [DAddOnConfig.AddOnData])
validateRequest merchantId sReq isOndcScheduledRideSupportEnabled = do
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mbEstimates <- mapM QEst.findById sReq.estimateIds
  let estimates = catMaybes mbEstimates
  case estimates of
    [] -> throwError $ InvalidRequest "User need to select at least one estimate"
    (estimate : xs) -> do
      searchReq <- QSR.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)
      -- Synchronous NACK, before any fork. Resolved once here and handed to 'handler' (which runs after this, inside its own fork) so it doesn't re-query for the same rows.
      addOnData <-
        if isOndcScheduledRideSupportEnabled
          then SAddOn.resolveAddOnData searchReq.merchantOperatingCityId (Just estimate.vehicleServiceTier) sReq.addOns
          else pure []
      return (merchant, searchReq, [estimate] <> xs, addOnData)

addNammaTags :: Y.SelectTagData -> DSR.SearchRequest -> Flow ()
addNammaTags tagData sReq = do
  newSearchTags <- withTryCatch "computeNammaTags:Select" (LYDL.computeNammaTagsWithDebugLog LYDL.Driver (cast sReq.merchantOperatingCityId) Yudhishthira.Select (Just sReq.transactionId) tagData)
  let tags = sReq.searchTags <> eitherToMaybe newSearchTags
  QSR.updateSearchTags tags sReq.id

-- ONDC scheduled-ride pilot: /select for the new Quote-based (static/scheduled) capability --
-- Dispatched only for BAPs with enableOndcScheduledRideSupport in this city,
-- at the API layer (API.Beckn.Select), when the wire item.id resolves to a Quote
-- instead of an Estimate.

-- | Validate a Quote-based /select. If the BAP sent a negotiated fare, this also
-- persists it on the quote (see 'applyNegotiatedFare'); otherwise it is a pure
-- read. No driver-search trigger (unlike 'handler' above, which is the
-- Estimate-based/dynamic-offer path).
-- Driver search for this flow already starts later, at /confirm
-- (Domain.Action.Beckn.Confirm.handleStaticOfferFlow).
validateQuoteSelect :: Id DM.Merchant -> Id DQuote.Quote -> DSelectReq -> Flow (DM.Merchant, DSR.SearchRequest, DQuote.Quote)
validateQuoteSelect merchantId quoteId sReq = do
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteNotFound quoteId.getId)
  now <- getCurrentTime
  unless (quote.validTill > now) $
    throwError $ QuoteExpired quoteId.getId
  searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
  unless (searchReq.transactionId == sReq.transactionId) $
    throwError $ InvalidRequest "select transaction_id does not match the search context this quote belongs to"
  quote' <- applyNegotiatedFare searchReq.merchantOperatingCityId quoteId sReq
  return (merchant, searchReq, quote')

-- | Validates and persists whatever /select actually sent -- the negotiated fare tolerance check (if a bid was made) and the add-on selection (if one was made) -- in a single update to the Quote row, instead of two separate ones.
applyNegotiatedFare :: Id DMOC.MerchantOperatingCity -> Id DQuote.Quote -> DSelectReq -> Flow DQuote.Quote
applyNegotiatedFare merchantOpCityId quoteId sReq =
  -- Concurrent /select calls on the same quote would race their reads/writes.
  -- This locks per quoteId and re-fetches the quote inside the lock.
  Redis.withLockRedisAndReturnValue (quoteNegotiationLockKey quoteId.getId) 60 $ do
    quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteNotFound quoteId.getId)
    addOnData <- computeAddOnData quote
    -- Priced once, here, and frozen into the quote's FareParameters: the catalogue is what's on offer, the fare parameters are what was charged.
    addOnCharges <- SAddOn.addOnChargesTotal addOnData
    let fareParams = quote.fareParams
        -- estimatedFare carries whatever an earlier /select already applied, so strip both parts off to recover the fare originally quoted. Everything below is computed from that, which is what keeps repeated /selects from ratcheting the fare.
        originalFare = quote.estimatedFare - fromMaybe 0 fareParams.negotiatedFareDelta - fromMaybe 0 fareParams.addOnCharges
    negotiatedFareDelta <- computeNegotiatedFareDelta quote originalFare
    let updatedFareParams =
          fareParams
            { DFareParams.negotiatedFareDelta = negotiatedFareDelta,
              DFareParams.addOnCharges = addOnCharges
            }
        -- The bid is the ride fare; add-on charges go on top of it.
        estimatedFare = originalFare + fromMaybe 0 negotiatedFareDelta + fromMaybe 0 addOnCharges
        updatedQuote =
          quote
            { DQuote.estimatedFare = estimatedFare,
              DQuote.fareParams = updatedFareParams,
              DQuote.addOnData = addOnData
            }
    QFareParams.updateFareParameters updatedFareParams fareParams.id
    QQuote.updateEstimatedFareAndAddOnDetails quoteId updatedQuote.estimatedFare updatedQuote.addOnData
    return updatedQuote
  where
    -- Tolerance lives on the quote's own FarePolicy (per-vehicle-tier), not TransporterConfig (city-level) -- a Sedan and an Auto on the same city can negotiate different bands. Defaults to +-10% when unset on the policy (see DFP.effectiveNegotiationTolerancePct).
    computeNegotiatedFareDelta quote originalFare = case sReq.negotiatedFare of
      -- No bid on this /select: keep whatever an earlier one negotiated.
      Nothing -> pure quote.fareParams.negotiatedFareDelta
      Just negotiatedFare -> do
        let negotiationFareMinTolerancePct = DFP.effectiveNegotiationTolerancePct (quote.farePolicy >>= (.negotiationFareMinTolerancePct))
            negotiationFareMaxTolerancePct = DFP.effectiveNegotiationTolerancePct (quote.farePolicy >>= (.negotiationFareMaxTolerancePct))
            negotiationFareMinToleranceFraction = realToFrac negotiationFareMinTolerancePct / 100
            negotiationFareMaxToleranceFraction = realToFrac negotiationFareMaxTolerancePct / 100
            minAcceptable = originalFare * (1 - negotiationFareMinToleranceFraction)
            maxAcceptable = originalFare * (1 + negotiationFareMaxToleranceFraction)
        unless (negotiatedFare >= minAcceptable && negotiatedFare <= maxAcceptable) $
          throwError $ NegotiatedFareNotAcceptable quoteId.getId negotiatedFare minAcceptable maxAcceptable
        -- Total delta from the original fare, not just this negotiation round's step.
        pure $ Just (negotiatedFare - originalFare)

    computeAddOnData quote
      | Kernel.Prelude.null sReq.addOns = pure [] -- if same request will come which already processed then we should process the latest addOns information not the esisting one
      | otherwise = SAddOn.resolveAddOnData merchantOpCityId (Just quote.vehicleServiceTier) sReq.addOns

quoteNegotiationLockKey :: Text -> Text
quoteNegotiationLockKey id = "Driver:Select:Negotiate:QuoteId-" <> id

-- | Build and send /on_select for a validated Quote (see
-- Beckn.OnDemand.Transformer.OndcScheduledRide.OnSelect for the builder). Called from a fork,
-- same as 'handler' above, using the inbound /select's own messageId -- unlike
-- the dynamic-offer flow's callOnSelectV2, there's no driver bid to wait for, so
-- this is synchronous within the same request, not deferred to a later event.
handleQuoteSelect :: Text -> DM.Merchant -> DSR.SearchRequest -> DQuote.Quote -> Flow ()
handleQuoteSelect msgId merchant searchReq quote = do
  now <- getCurrentTime
  let vehicleCategory = BUtils.mapServiceTierToCategory quote.vehicleServiceTier
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
  vehicleServiceTierItem <-
    CQVST.findByServiceTierTypeAndCityIdInRideFlow quote.vehicleServiceTier searchReq.merchantOperatingCityId (searchReq.area >>= SL.pickupSpecialZoneIdFromArea)
      >>= fromMaybeM (VehicleServiceTierNotFound (show quote.vehicleServiceTier))
  mbFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback quote.id.getId
  isValueAddNP <- CQVAN.isValueAddNP searchReq.bapId
  onSelectMsg <- OSROnSelect.mkOnSelectMessageV2FromQuote isValueAddNP bppConfig merchant searchReq quote vehicleServiceTierItem mbFarePolicy now
  CallBAP.callOnSelectV2ForQuote merchant searchReq msgId quote onSelectMsg
