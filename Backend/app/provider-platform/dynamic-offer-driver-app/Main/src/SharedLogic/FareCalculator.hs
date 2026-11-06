{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module SharedLogic.FareCalculator
  ( fareSum,
    netRideFare,
    FareParamsAmounts (..),
    fareParamsAmounts,
    customerCancellationFareBasis,
    perRideKmFareParamsSum,
    getPerMinuteRate,
    CalculateFareParametersParams (..),
    FareComputationPhase (..),
    isNightShift,
    isNightAllowanceApplicable,
    timeZoneIST,
    UTCTime (UTCTime, utctDay),
    computeRideDiscount,
    computeTotalGstRate,
    countFullFareOfParamsDetails,
    interCityReservedTime,
    calculateFareParameters,
    calculateCommission,
    CommissionBreakup (..),
    commissionOnComponent,
    applyPaymentChargeDetails,
    paymentChargeQuoteFor,
    paymentChargeResult,
    PaymentChargeResult (..),
    driverBorneAppFee,
    driverEffectiveFare,
    customerBorneCharge,
    mkFareParamsBreakups,
    cancellationTaxLines,
    buildComponentMap,
    componentAmount,
    clampDiscountToDiscountable,
    ComponentMap,
    entryFeeForGateId,
    parseCodeValue,
    ParsedCodeValue (..),
  )
where

import "this" API.Types.ProviderPlatform.Management.Merchant hiding (NightShiftChargeAPIEntity (..), VehicleVariant (..), WaitingChargeAPIEntity (..))
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import BecknV2.OnDemand.FareBreakup.Tax (FareTaxes, TaxBreakup, taxTotal)
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.PaymentCharge (PaymentChargeDetails, PaymentChargeQuote (..), customerPaymentChargeGross, paymentChargeGross, quotePaymentCharge)
import Data.Char (isDigit)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Domain.Types.Common
import qualified Domain.Types.ConditionalCharges as DAC
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as DFP
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import qualified Domain.Types.TransporterConfig as DTC
import EulerHS.Prelude hiding (elem, id, map, sum)
import GHC.Float (int2Double)
import Kernel.Prelude as KP
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Price as Price
import Kernel.Utils.Common hiding (isTimeWithinBounds, mkPrice)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Types.GateInfoExtra as DGI
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.RideTax as RideTax
import Storage.Beam.SpecialZone ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

cancellationDuesTaxExclusive :: FareParameters -> Maybe HighPrecMoney
cancellationDuesTaxExclusive fp = fp.customerCancellationDuesTaxExclusive <|> fp.customerCancellationDues

cancellationTaxLines :: Maybe TaxBreakup -> [(Text, HighPrecMoney)]
cancellationTaxLines = componentTaxLines ((.cancellationFee) FB.taxTags)

fareTaxLines :: FareTaxes -> [(Text, HighPrecMoney)]
fareTaxLines taxes = concat (componentTaxLines <$> FB.taxTags <*> taxes)

componentTaxLines :: FB.TaxTags -> Maybe TaxBreakup -> [(Text, HighPrecMoney)]
componentTaxLines tags = \case
  Nothing -> []
  Just t -> titled tags.tax (Just t.total) <> titled tags.cgst t.cgst <> titled tags.sgst t.sgst <> titled tags.igst t.igst
  where
    titled tag = maybeToList . fmap (FB.tagTitle tag,) . mfilter (/= 0)

-- | Three components carry an extra copy under their ONDC v2.1.0 spec title
--   (@PARKING_CHARGES@, @NIGHT_CHARGES@, @WAITING_CHARGES@). Those exist purely
--   for external NPs that validate against the spec vocabulary, so they are
--   emitted only when the BAP is not a value-add NP — our own apps read the
--   legacy titles, and sending both would show the component twice.
mkFareParamsBreakups :: Bool -> (HighPrecMoney -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkFareParamsBreakups isValueAddNP mkPrice mkBreakupItem fareParams = do
  -- let dayPartRate = fromMaybe 1.0 fareParams.nightShiftRateIfApplies -- Temp fix :: have to fix properly
  let baseFareFinal = HighPrecMoney $ fareParams.baseFare.getHighPrecMoney -- Temp fix :: have to fix properly
      baseFareCaption = show Enums.BASE_FARE
      baseFareItem = mkBreakupItem baseFareCaption (mkPrice baseFareFinal)

      -- Its own breakup line, since the breakup total otherwise diverges from the negotiated price.
      negotiatedFareDeltaCaption = show Enums.NEGOTIATED_FARE_DELTA
      mbNegotiatedFareDeltaItem = mkBreakupItem negotiatedFareDeltaCaption . mkPrice <$> fareParams.negotiatedFareDelta

      -- Its own breakup line, so the rider can see what the add-ons cost and the breakup still sums to the fare.
      addOnChargesCaption = show Enums.ADD_ON_CHARGES
      mbAddOnChargesItem = mkBreakupItem addOnChargesCaption . mkPrice <$> fareParams.addOnCharges

      serviceChargeCaption = show Enums.SERVICE_CHARGE
      mbServiceChargeItem = fmap (mkBreakupItem serviceChargeCaption) (mkPrice <$> fareParams.serviceCharge)

      congestionChargeCaption = show Enums.CONGESTION_CHARGE
      mbCongestionChargeItem = fmap (mkBreakupItem congestionChargeCaption) (mkPrice . (+ fromMaybe 0 fareParams.negativeFareAdjustment) <$> fareParams.congestionCharge)

      mkSelectedFareCaption = show Enums.DRIVER_SELECTED_FARE
      mbSelectedFareItem =
        fareParams.driverSelectedFare <&> \selFare ->
          mkBreakupItem mkSelectedFareCaption (mkPrice selFare)

      customerExtraFareCaption = show Enums.CUSTOMER_SELECTED_FARE
      mkCustomerExtraFareItem =
        fareParams.customerExtraFee <&> \ceFare -> do
          mkBreakupItem customerExtraFareCaption (mkPrice ceFare)

      extraTimeFareCaption = show Enums.EXTRA_TIME_FARE
      mkExtraTimeFareCaption =
        fareParams.rideExtraTimeFare <&> \tbCharge -> do
          mkBreakupItem extraTimeFareCaption (mkPrice tbCharge)

      -- This is not supposed to be passed separately as it is already passed in Base Fare hence in the filter it is removed
      nightShiftCaption = show Enums.NIGHT_SHIFT_CHARGE
      mbNightShiftChargeItem = fmap (mkBreakupItem nightShiftCaption) (mkPrice <$> fareParams.nightShiftCharge)

      -- ONDC v2.1.0: duplicate with new title for spec compliance, external NPs only
      nightChargesCaption = show Enums.NIGHT_CHARGES
      mbNightChargesItem = mkV2SpecAliasItem nightChargesCaption fareParams.nightShiftCharge

      parkingChargeCaption = show Enums.PARKING_CHARGE
      mbParkingChargeItem = mkBreakupItem parkingChargeCaption . mkPrice <$> fareParams.parkingCharge

      -- ONDC v2.1.0: duplicate with new title for spec compliance, external NPs only
      parkingChargesCaption = show Enums.PARKING_CHARGES
      mbParkingChargesItem = mkV2SpecAliasItem parkingChargesCaption fareParams.parkingCharge

      waitingChargesCaption = show Enums.WAITING_OR_PICKUP_CHARGES
      mbWaitingChargesItem = mkBreakupItem waitingChargesCaption . mkPrice <$> fareParams.waitingCharge

      -- ONDC v2.1.0: duplicate with new title for spec compliance, external NPs only
      waitingChargesCaption2 = show Enums.WAITING_CHARGES
      mbWaitingChargesItem2 = mkV2SpecAliasItem waitingChargesCaption2 fareParams.waitingCharge

      customerCancellationDuesCaption = show Enums.CANCELLATION_CHARGES
      mbCustomerCancellationDues = mkBreakupItem customerCancellationDuesCaption . mkPrice <$> cancellationDuesTaxExclusive fareParams

      gateFeeTotal = customerGateFeeItemsSum fareParams
      airportEntryFeeCaption = show Enums.AIRPORT_ENTRY_FEE
      airportEntryFeeAmount = fromMaybe 0 fareParams.airportEntryFee + (if isValueAddNP then 0 else gateFeeTotal)
      mbAirportEntryFeeItem = if airportEntryFeeAmount /= 0 then Just (mkBreakupItem airportEntryFeeCaption (mkPrice airportEntryFeeAmount)) else Nothing

      taxItems = map (\(t, amt) -> mkBreakupItem t (mkPrice amt)) taxBreakupLines

      tollChargesCaption = show Enums.TOLL_CHARGES
      mbTollChargesItem = mkBreakupItem tollChargesCaption . mkPrice <$> fareParams.tollCharges

      petChargesCaption = show Enums.PET_CHARGES
      mbPetChargesItem = mkBreakupItem petChargesCaption . mkPrice <$> fareParams.petCharges

      driverAllowanceCaption = show Enums.DRIVER_ALLOWANCE
      mbDriverAllowanceItem = mkBreakupItem driverAllowanceCaption . mkPrice <$> fareParams.driverAllowance

      airportConvenienceFeeCaption = show Enums.AIRPORT_CONVENIENCE_FEE
      mbAirportConvenienceFeeItem = mkBreakupItem airportConvenienceFeeCaption . mkPrice <$> fareParams.airportConvenienceFee

      mkBusinessDiscountCaption = show Enums.BUSINESS_DISCOUNT
      mbBusinessDiscountItem = if fareParams.shouldApplyBusinessDiscount then mkBreakupItem mkBusinessDiscountCaption . mkPrice . negate <$> fareParams.businessDiscount else Nothing

      mkPersonalDiscountCaption = show Enums.PERSONAL_DISCOUNT
      mbPersonalDiscountItem = if fareParams.shouldApplyPersonalDiscount then mkBreakupItem mkPersonalDiscountCaption . mkPrice . negate <$> fareParams.personalDiscount else Nothing

      priorityChargesCaption = show Enums.PRIORITY_CHARGES
      mbPriorityChargesItem = mkBreakupItem priorityChargesCaption . mkPrice <$> fareParams.priorityCharges

      insuranceChargeCaption = show Enums.INSURANCE_CHARGES
      mbInsuranceChargeItem = mkBreakupItem insuranceChargeCaption . mkPrice <$> fareParams.insuranceCharge

      cardChargesFareCaption = show Enums.CARD_CHARGES_ON_FARE
      mbCardChargesFareItem = fareParams.cardCharge >>= \cardCharge -> mkBreakupItem cardChargesFareCaption . mkPrice <$> cardCharge.onFare

      cardChargesFixedCaption = show Enums.CARD_CHARGES_FIXED
      mbCardChargesFixedItem = fareParams.cardCharge >>= \cardCharge -> mkBreakupItem cardChargesFixedCaption . mkPrice <$> cardCharge.fixed

      -- Stripe payment charge shown to the customer (only when the rider bears it —
      -- 'paymentProcessingFee' is populated only in that case by 'calculateFareParameters').
      paymentChargeCaption = show Enums.PAYMENT_CHARGE
      mbPaymentChargeItem = mkBreakupItem paymentChargeCaption . mkPrice <$> fareParams.paymentProcessingFee

      rideStopChargeCaption = show Enums.RIDE_STOP_CHARGES
      mbRideStopChargeItem = mkBreakupItem rideStopChargeCaption . mkPrice <$> fareParams.stopCharges

      luggageChargeCaption = show Enums.LUGGAGE_CHARGE
      mbLuggageChargeItem = mkBreakupItem luggageChargeCaption . mkPrice <$> fareParams.luggageCharge

      returnFeeChargeCaption = show Enums.RETURN_FEE
      mbReturnFeeChargeItem = mkBreakupItem returnFeeChargeCaption . mkPrice <$> fareParams.returnFeeCharge

      boothChargeCaption = show Enums.BOOTH_CHARGE
      mbBoothChargeItem = mkBreakupItem boothChargeCaption . mkPrice <$> fareParams.boothCharge

      schedulingChargeCaption = show Enums.SCHEDULING_CHARGE
      mbSchedulingChargeItem = mkBreakupItem schedulingChargeCaption . mkPrice <$> fareParams.schedulingCharge

      detailsBreakups = processFareParamsDetails fareParams.fareParametersDetails
      additionalChargesBreakup = map (\addCharges -> mkBreakupItem (show $ castAdditionalChargeCategoriesToEnum addCharges.chargeCategory) $ mkPrice addCharges.charge) fareParams.conditionalCharges
      -- The itemName is operator-configured free text, not an ONDC v2.1.0 breakup
      -- title, so this line goes only to our own apps -- an external NP validating
      -- against the spec vocabulary would reject it.
      gateFeeItemsBreakup =
        if isValueAddNP
          then map (\item -> mkBreakupItem (Enums.mkGateFeeBreakupTitle item.itemName) $ mkPrice item.amount) (fromMaybe [] fareParams.customerGateFeeItems)
          else []
  catMaybes
    [ Just baseFareItem,
      mbNegotiatedFareDeltaItem,
      mbAddOnChargesItem,
      mbCongestionChargeItem,
      mbNightShiftChargeItem,
      mbNightChargesItem,
      mbParkingChargeItem,
      mbParkingChargesItem,
      mbWaitingChargesItem,
      mbWaitingChargesItem2,
      mbBusinessDiscountItem,
      mbPersonalDiscountItem,
      mbPetChargesItem,
      mbDriverAllowanceItem,
      mbAirportConvenienceFeeItem,
      mbPriorityChargesItem,
      mbServiceChargeItem,
      mbSelectedFareItem,
      mkCustomerExtraFareItem,
      mkExtraTimeFareCaption,
      mbTollChargesItem,
      mbCustomerCancellationDues,
      mbInsuranceChargeItem,
      mbCardChargesFareItem,
      mbCardChargesFixedItem,
      mbPaymentChargeItem,
      mbRideStopChargeItem,
      mbLuggageChargeItem,
      mbReturnFeeChargeItem,
      mbBoothChargeItem,
      mbSchedulingChargeItem,
      mbAirportEntryFeeItem
    ]
    <> detailsBreakups
    <> additionalChargesBreakup
    <> gateFeeItemsBreakup
    <> taxItems
  where
    -- The ONDC v2.1.0 spec title for a component we already emit under its legacy
    -- title. Only external NPs need it; for our own apps it would be a duplicate.
    mkV2SpecAliasItem caption mbAmount =
      if isValueAddNP
        then Nothing
        else mkBreakupItem caption . mkPrice <$> mbAmount

    castAdditionalChargeCategoriesToEnum = \case
      DAC.SAFETY_PLUS_CHARGES -> Enums.SAFETY_PLUS_CHARGES
      DAC.NYREGULAR_SUBSCRIPTION_CHARGE -> Enums.NYREGULAR_SUBSCRIPTION_CHARGE
      _ -> Enums.NO_CHARGES
    processFareParamsDetails = \case
      DFParams.ProgressiveDetails det -> mkFPProgressiveDetailsBreakupList det
      DFParams.SlabDetails det -> mkFPSlabDetailsBreakupList det
      DFParams.RentalDetails det -> mkFPRentalDetailsBreakupList det
      DFParams.InterCityDetails det -> mkFPInterCityDetailsBreakupList det
      DFParams.AmbulanceDetails det -> mkFPAmbulanceDetailsBreakupList det

    mkFPProgressiveDetailsBreakupList det = do
      let deadKmFareCaption = show Enums.DEAD_KILOMETER_FARE
          deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice det.deadKmFare)

          extraDistanceFareCaption = show Enums.DISTANCE_FARE
          mbExtraKmFare = det.extraKmFare
          extraDistanceFareItem =
            mbExtraKmFare <&> \extraKmFareRounded ->
              mkBreakupItem extraDistanceFareCaption (mkPrice extraKmFareRounded) -- temp fix :: MOVE CONGESTION CHARGE TO PROPER PLACE
          rideDurationFareCaption = show Enums.RIDE_DURATION_FARE
          mbRideDurationFareItem = det.rideDurationFare <&> mkBreakupItem rideDurationFareCaption . mkPrice

      catMaybes [Just deadKmFareItem, extraDistanceFareItem, mbRideDurationFareItem]

    mkFPSlabDetailsBreakupList det = do
      let platformFeeCaption = show Enums.PLATFORM_FEE
          mbPlatformFeeItem = mkBreakupItem platformFeeCaption . mkPrice <$> det.platformFee
          sgstCaption = show Enums.PLATFORM_FEE_SGST
          mbSgstItem = mkBreakupItem sgstCaption . mkPrice <$> det.sgst
          cgstCaption = show Enums.PLATFORM_FEE_CGST
          mbCgstItem = mkBreakupItem cgstCaption . mkPrice <$> det.cgst
          mbVatItem = mkBreakupItem (show Enums.PLATFORM_FEE_VAT) . mkPrice <$> det.vat
      catMaybes [mbPlatformFeeItem, mbSgstItem, mbCgstItem, mbVatItem]

    mkFPRentalDetailsBreakupList det = do
      let timeBasedFareCaption = show Enums.TIME_BASED_FARE
          mbTimeBasedFare = mkBreakupItem timeBasedFareCaption (mkPrice det.timeBasedFare)
          distBasedCaption = show Enums.DIST_BASED_FARE
          mbDistBasedFare = mkBreakupItem distBasedCaption (mkPrice det.distBasedFare)
          deadKmFareCaption = show Enums.DEAD_KILOMETER_FARE
          mbDeadKmFare = mkBreakupItem deadKmFareCaption . mkPrice <$> checkIfZero det.deadKmFare
      catMaybes [Just mbTimeBasedFare, Just mbDistBasedFare, mbDeadKmFare]

    mkFPAmbulanceDetailsBreakupList det = do
      let platformFeeCaption = show Enums.PLATFORM_FEE
          mbPlatformFeeItem = mkBreakupItem platformFeeCaption . mkPrice <$> det.platformFee
          sgstCaption = show Enums.PLATFORM_FEE_SGST
          mbSgstItem = mkBreakupItem sgstCaption . mkPrice <$> det.sgst
          cgstCaption = show Enums.PLATFORM_FEE_CGST
          mbCgstItem = mkBreakupItem cgstCaption . mkPrice <$> det.cgst
          mbVatItem = mkBreakupItem (show Enums.PLATFORM_FEE_VAT) . mkPrice <$> det.vat
          distBasedCaption = show Enums.DIST_BASED_FARE
          mbDistBasedFare = mkBreakupItem distBasedCaption (mkPrice det.distBasedFare)
      catMaybes [mbPlatformFeeItem, mbSgstItem, mbCgstItem, mbVatItem, Just mbDistBasedFare]

    mkFPInterCityDetailsBreakupList det = do
      let deadKmFareCaption = show Enums.DEAD_KILOMETER_FARE
          deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice det.pickupCharge)

          timeBasedFareCaption = show Enums.TIME_BASED_FARE
          mbTimeBasedFare = mkBreakupItem timeBasedFareCaption (mkPrice det.timeFare)

          distBasedCaption = show Enums.DIST_BASED_FARE
          mbDistBasedFare = mkBreakupItem distBasedCaption (mkPrice det.distanceFare)

          extraDistanceCaption = show Enums.EXTRA_DISTANCE_FARE
          extraDistanceFareItem = mkBreakupItem extraDistanceCaption (mkPrice det.extraDistanceFare)

          extraTimeCaption = show Enums.EXTRA_TIME_FARE
          extraTimeFareItem = mkBreakupItem extraTimeCaption (mkPrice det.extraTimeFare)

          stateEntryPermitChargesCaption = show Enums.STATE_ENTRY_PERMIT_CHARGES
          mbStateEntryPermitChargesItem = mkBreakupItem stateEntryPermitChargesCaption . mkPrice <$> det.stateEntryPermitCharges

      catMaybes [Just deadKmFareItem, Just mbTimeBasedFare, Just mbDistBasedFare, Just extraDistanceFareItem, Just extraTimeFareItem, mbStateEntryPermitChargesItem]

    checkIfZero fare =
      if fare > 0
        then Just fare
        else Nothing

    taxBreakupLines :: [(Text, HighPrecMoney)]
    taxBreakupLines = fareTaxLines (RideTax.fareTaxesOf fareParams)

withFareTaxes :: FareTaxes -> FareParameters -> FareParameters
withFareTaxes taxes fp =
  fp
    { govtCharges = Tax.total <$> taxes.rideFare,
      rideTaxCgst = taxes.rideFare >>= Tax.cgst,
      rideTaxSgst = taxes.rideFare >>= Tax.sgst,
      rideTaxIgst = taxes.rideFare >>= Tax.igst,
      tollFareTax = Tax.total <$> taxes.toll,
      tollTaxCgst = taxes.toll >>= Tax.cgst,
      tollTaxSgst = taxes.toll >>= Tax.sgst,
      tollTaxIgst = taxes.toll >>= Tax.igst,
      parkingChargeTax = Tax.total <$> taxes.parking,
      parkingTaxCgst = taxes.parking >>= Tax.cgst,
      parkingTaxSgst = taxes.parking >>= Tax.sgst,
      parkingTaxIgst = taxes.parking >>= Tax.igst,
      airportEntryFeeTax = Tax.total <$> taxes.airportEntryFee,
      airportEntryFeeTaxCgst = taxes.airportEntryFee >>= Tax.cgst,
      airportEntryFeeTaxSgst = taxes.airportEntryFee >>= Tax.sgst,
      airportEntryFeeTaxIgst = taxes.airportEntryFee >>= Tax.igst,
      customerCancellationDuesTax = Tax.total <$> taxes.cancellationFee,
      cancellationTaxCgst = taxes.cancellationFee >>= Tax.cgst,
      cancellationTaxSgst = taxes.cancellationFee >>= Tax.sgst,
      cancellationTaxIgst = taxes.cancellationFee >>= Tax.igst,
      paymentProcessingFeeTax = Tax.total <$> taxes.paymentCharge,
      paymentChargeTaxCgst = taxes.paymentCharge >>= Tax.cgst,
      paymentChargeTaxSgst = taxes.paymentCharge >>= Tax.sgst,
      paymentChargeTaxIgst = taxes.paymentCharge >>= Tax.igst
    }

-- TODO: make some tests for it

customerGateFeeItemsSum :: FareParameters -> HighPrecMoney
customerGateFeeItemsSum fareParams = sum $ map (.amount) (fromMaybe [] fareParams.customerGateFeeItems)

-- | NOTE: every component summed here must be mapped in
--   'buildComponentMap'/'rebuildWithComponents' so the fare-recompute cap stays
--   enforceable. Adding a record field breaks those constructions — decide the
--   new component's cap strategy when you do.
fareSum :: FareParameters -> Maybe [DAC.ConditionalChargesCategories] -> HighPrecMoney
fareSum fareParams conditionalChargeCategories =
  pureFareSum
    + fromMaybe 0.0 fareParams.driverSelectedFare
    + fromMaybe 0.0 fareParams.customerExtraFee
    + fromMaybe 0.0 fareParams.negotiatedFareDelta
    + fromMaybe 0.0 fareParams.negativeFareAdjustment
    - (if fareParams.shouldApplyBusinessDiscount then fromMaybe 0.0 fareParams.businessDiscount else 0.0)
    - (if fareParams.shouldApplyPersonalDiscount then fromMaybe 0.0 fareParams.personalDiscount else 0.0)
  where
    edcCollectsParking :: Bool
    edcCollectsParking = SL.edcCollectsParking fareParams.fareSettlementType

    pureFareSum :: HighPrecMoney
    pureFareSum = do
      let (partOfNightShiftCharge, notPartOfNightShiftCharge, platformFee) = countFullFareOfParamsDetails fareParams.fareParametersDetails
          parkingContribution = if edcCollectsParking then 0.0 else fromMaybe 0.0 fareParams.parkingCharge
          parkingTaxContribution = if edcCollectsParking then 0.0 else fromMaybe 0.0 fareParams.parkingChargeTax
          airportEntryFeeContribution = if edcCollectsParking then 0.0 else fromMaybe 0.0 fareParams.airportEntryFee + fromMaybe 0.0 fareParams.airportEntryFeeTax
      fareParams.baseFare
        + fromMaybe 0.0 fareParams.serviceCharge
        + fromMaybe 0.0 fareParams.waitingCharge
        + fromMaybe 0.0 fareParams.govtCharges
        + fromMaybe 0.0 fareParams.nightShiftCharge
        + fromMaybe 0.0 fareParams.rideExtraTimeFare
        + fromMaybe 0.0 fareParams.congestionCharge
        + fromMaybe 0.0 fareParams.petCharges
        + fromMaybe 0.0 fareParams.addOnCharges
        + fromMaybe 0.0 fareParams.driverAllowance
        + fromMaybe 0.0 fareParams.airportConvenienceFee
        + fromMaybe 0.0 fareParams.stopCharges
        + fromMaybe 0.0 fareParams.priorityCharges
        + partOfNightShiftCharge
        + notPartOfNightShiftCharge
        + platformFee
        + (fromMaybe 0.0 fareParams.customerCancellationDues + fromMaybe 0.0 fareParams.tollCharges + parkingContribution)
        + fromMaybe 0.0 fareParams.insuranceCharge
        + fromMaybe 0.0 fareParams.luggageCharge
        + fromMaybe 0.0 fareParams.returnFeeCharge
        + fromMaybe 0.0 fareParams.boothCharge
        + fromMaybe 0.0 fareParams.schedulingCharge
        + fromMaybe 0.0 (fareParams.cardCharge >>= (.onFare))
        + fromMaybe 0.0 (fareParams.cardCharge >>= (.fixed))
        + fromMaybe 0.0 fareParams.paymentProcessingFee
        + fromMaybe 0.0 fareParams.paymentProcessingFeeTax
        + fromMaybe 0.0 fareParams.tollFareTax
        + parkingTaxContribution
        + airportEntryFeeContribution
        -- Commission is intentionally excluded - stored for breakdown only
        + (sum $ map (.charge) (filter (\addCharges -> maybe True (KP.elem addCharges.chargeCategory) conditionalChargeCategories) fareParams.conditionalCharges))
        + customerGateFeeItemsSum fareParams

netRideFare :: FareParameters -> HighPrecMoney -> HighPrecMoney
netRideFare fareParams total =
  max 0 $
    total
      - fromMaybe 0.0 fareParams.govtCharges
      - fromMaybe 0.0 fareParams.tollCharges
      - fromMaybe 0.0 fareParams.tollFareTax
      - parkingAmount
      - parkingTaxAmount
      - airportEntryFeeAmount
  where
    edcParkingCollected = SL.edcCollectsParking fareParams.fareSettlementType
    parkingAmount = if edcParkingCollected then 0.0 else fromMaybe 0.0 fareParams.parkingCharge
    parkingTaxAmount = if edcParkingCollected then 0.0 else fromMaybe 0.0 fareParams.parkingChargeTax
    airportEntryFeeAmount = if edcParkingCollected then 0.0 else fromMaybe 0.0 fareParams.airportEntryFee + fromMaybe 0.0 fareParams.airportEntryFeeTax

data FareParamsAmounts = FareParamsAmounts
  { rideBase :: HighPrecMoney,
    rideTax :: HighPrecMoney,
    toll :: HighPrecMoney,
    tollTax :: HighPrecMoney,
    parking :: HighPrecMoney,
    parkingTax :: HighPrecMoney,
    airportFee :: HighPrecMoney,
    airportFeeTax :: HighPrecMoney,
    platformFeeTotal :: HighPrecMoney,
    cancellationDues :: HighPrecMoney
  }

fareParamsAmounts :: Maybe HighPrecMoney -> FareParameters -> FareParamsAmounts
fareParamsAmounts mbRideFare fp =
  FareParamsAmounts
    { rideBase = fromMaybe (fareSum fp Nothing) mbRideFare - taxOfRide - others,
      rideTax = taxOfRide,
      toll = tollAmt,
      tollTax = tollTaxAmt,
      parking = parkingAmt,
      parkingTax = parkingTaxAmt,
      airportFee = airportAmt,
      airportFeeTax = airportTaxAmt,
      platformFeeTotal = platformTotal,
      cancellationDues = duesAmt
    }
  where
    edc = SL.edcCollectsParking fp.fareSettlementType
    unlessEdc amt = if edc then 0 else amt
    taxOfRide = fromMaybe 0 fp.govtCharges
    tollAmt = fromMaybe 0 fp.tollCharges
    tollTaxAmt = fromMaybe 0 fp.tollFareTax
    parkingAmt = unlessEdc (fromMaybe 0 fp.parkingCharge)
    parkingTaxAmt = unlessEdc (fromMaybe 0 fp.parkingChargeTax)
    airportAmt = unlessEdc (fromMaybe 0 fp.airportEntryFee) + customerGateFeeItemsSum fp
    airportTaxAmt = unlessEdc (fromMaybe 0 fp.airportEntryFeeTax)
    (_, _, platformTotal) = countFullFareOfParamsDetails fp.fareParametersDetails
    duesAmt = fromMaybe 0 fp.customerCancellationDues
    paymentChargeAmt = fromMaybe 0 fp.paymentProcessingFee + fromMaybe 0 fp.paymentProcessingFeeTax
    others = tollAmt + tollTaxAmt + parkingAmt + parkingTaxAmt + airportAmt + airportTaxAmt + platformTotal + duesAmt + paymentChargeAmt

customerCancellationFareBasis :: FareParameters -> HighPrecMoney -> HighPrecMoney
customerCancellationFareBasis fareParams total =
  max 0 (netRideFare fareParams total - fromMaybe 0.0 fareParams.customerCancellationDues)

perRideKmFareParamsSum :: FareParameters -> HighPrecMoney
perRideKmFareParamsSum fareParams = do
  let (partOfNightShiftCharge, notPartOfNightShiftCharge, _) = countFullFareOfParamsDetails fareParams.fareParametersDetails
  fareParams.baseFare
    + partOfNightShiftCharge
    + notPartOfNightShiftCharge
    -- + fromMaybe 0.0 fareParams.congestionCharge -- removing congestion charge from rate card
    + fromMaybe 0.0 fareParams.nightShiftCharge

getPerMinuteRate :: FareParameters -> Maybe PriceAPIEntity
getPerMinuteRate fareParams = do
  case fareParams.fareParametersDetails of
    DFParams.ProgressiveDetails det -> do
      mkPriceAPIEntity . Price.mkPrice (Just det.currency) <$> det.rideDurationFare
    _ -> Nothing

data FareComputationPhase = FCEstimate | FCRecompute | FCBuffer
  deriving (Eq, Show)

data CalculateFareParametersParams = CalculateFareParametersParams
  { farePolicy :: FullFarePolicy,
    computationPhase :: FareComputationPhase,
    actualDistance :: Maybe Meters,
    rideTime :: UTCTime,
    waitingTime :: Maybe Minutes,
    stopWaitingTimes :: [Minutes],
    returnTime :: Maybe UTCTime,
    vehicleAge :: Maybe Months,
    roundTrip :: Bool,
    actualRideDuration :: Maybe Seconds,
    driverSelectedFare :: Maybe HighPrecMoney,
    customerExtraFee :: Maybe HighPrecMoney,
    negativeFareAdjustment :: Maybe HighPrecMoney,
    nightShiftCharge :: Maybe HighPrecMoney,
    customerCancellationDues :: Maybe HighPrecMoney,
    customerCancellationDuesTaxBreakup :: Maybe TaxBreakup,
    customerCancellationDuesCommission :: Maybe HighPrecMoney,
    customerCancellationDuesCommissionTaxBreakup :: Maybe TaxBreakup,
    estimatedRideDuration :: Maybe Seconds,
    estimatedRideStaticDuration :: Maybe Seconds, -- traffic-free duration from the routing provider; basis for TrafficDelayDuration pricing
    estimatedCongestionCharge :: Maybe HighPrecMoney,
    isScheduled :: Bool,
    nightShiftOverlapChecking :: Bool,
    estimatedDistance :: Maybe Meters,
    timeDiffFromUtc :: Maybe Seconds,
    tollCharges :: Maybe HighPrecMoney,
    isManualTollCharge :: Bool,
    noOfStops :: Int,
    currency :: Currency,
    distanceUnit :: DistanceUnit,
    petCharges :: Maybe HighPrecMoney,
    -- | Charge for the rider add-ons selected at /select, passed through unchanged (outside discounts; VAT/commission via AddOnChargeComponent in the fare policy) -- see FareParameters.addOnCharges.
    addOnCharges :: Maybe HighPrecMoney,
    -- | Delta of the fare negotiated at /select from the originally quoted fare, passed through unchanged so a recalculation does not drop it -- see FareParameters.negotiatedFareDelta.
    negotiatedFareDelta :: Maybe HighPrecMoney,
    shouldApplyBusinessDiscount :: Bool,
    shouldApplyPersonalDiscount :: Bool,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    mbAdditonalChargeCategories :: Maybe [DAC.ConditionalChargesCategories],
    numberOfLuggages :: Maybe Int,
    pickupGateId :: Maybe Text, -- Optional airport pickup gate id; used by V2 to apply airport entry fee
    fareSettlementType :: Maybe SL.FareSettlementType,
    isParkingFeeExempt :: Bool,
    -- | Set together, only on the 'FCRecompute'/'FCBuffer' paths: every
    -- component is capped (or buffered) against 'mbEstimateFareParams' as it
    -- is computed, so 'fullRideCostN' and 'govtCharges' follow automatically.
    mbCapConfig :: Maybe FareRecomputeCapConfig,
    paymentChargeDetails :: Maybe PaymentChargeDetails,
    buyerOfferDiscount :: Maybe HighPrecMoney,
    mbEstimateFareParams :: Maybe FareParameters
  }

calculateFareParametersHandler :: MonadFlow m => CalculateFareParametersParams -> m FareParameters
calculateFareParametersHandler params = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| params.farePolicy.merchantId ||+ " and vehicle service tier " +|| params.farePolicy.vehicleServiceTier ||+ ""
  now <- getCurrentTime
  let fp = params.farePolicy
      rideEndTime = case (params.actualRideDuration, params.estimatedRideDuration) of
        (Just duration, _) -> addUTCTime (secondsToNominalDiffTime duration) params.rideTime
        (_, Just duration) -> addUTCTime (secondsToNominalDiffTime duration) params.rideTime
        _ -> now
      rideDur = fromMaybe 1 $ params.estimatedRideDuration <|> params.actualRideDuration
  id <- generateGUID
  let localTimeZoneSeconds = fromMaybe 19800 params.timeDiffFromUtc
  let rideTimeWithBuffer = addUTCTime (secondsToNominalDiffTime (fromMaybe 0 fp.pickupBufferInSecsForNightShiftCal)) params.rideTime
  let nightShiftBuffer = if isJust fp.pickupBufferInSecsForNightShiftCal then fromMaybe False (isNightShiftWithPickupBuffer <$> fp.nightShiftBounds <*> Just rideTimeWithBuffer <*> Just rideEndTime) else False
  logDebug $ "NightShiftChanges : " <> "NightShiftBuffer: " <> show nightShiftBuffer
  let isNightShiftChargeIncluded = nightShiftBuffer || fromMaybe nightShiftBuffer (if params.nightShiftOverlapChecking then Just $ isNightAllowanceApplicable fp.nightShiftBounds rideTimeWithBuffer rideEndTime localTimeZoneSeconds else isNightShift <$> fp.nightShiftBounds <*> Just params.rideTime)
      (debugLogs, rawBaseFare, nightShiftCharge, waitingChargeInfo, fareParametersDetails) = processFarePolicyDetails fp.farePolicyDetails
      baseFare = capComponent RideFare rawBaseFare
      (partOfNightShiftCharge, notPartOfNightShiftCharge, _) = countFullFareOfParamsDetails fareParametersDetails
      fullRideCost {-without govtCharges, serviceCharge, platformFee, waitingCharge, notPartOfNightShiftCharge, nightShift, insuranceCharge, cardChargeOnFare and fixedCardCharge-} =
        baseFare
          + partOfNightShiftCharge
  let resultFullNightShiftCharge = if isNightShiftChargeIncluded then countNightShiftCharge fullRideCost <$> nightShiftCharge else Nothing
  logDebug $ "NightShiftChanges : " <> "resultFullNightShiftCharge: " <> show resultFullNightShiftCharge
  let resultNightShiftCharge = capComponentMb NightShiftChargeComponent $ if nightShiftBuffer then calNightShiftCharge resultFullNightShiftCharge rideTimeWithBuffer fp.nightShiftBounds rideDur else resultFullNightShiftCharge
  logDebug $ "NightShiftChanges : " <> "resultNightShiftCharge: " <> show resultNightShiftCharge
  let resultWaitingCharge = capComponentMb WaitingCharge $ countWaitingCharge =<< waitingChargeInfo
      congestionChargeByMultiplier =
        fp.congestionChargeMultiplier <&> \case
          DFP.BaseFareAndExtraDistanceFare congestionCharge -> HighPrecMoney (fullRideCost.getHighPrecMoney * toRational congestionCharge) - fullRideCost
          DFP.ExtraDistanceFare congestionCharge -> HighPrecMoney (partOfNightShiftCharge.getHighPrecMoney * toRational congestionCharge) - partOfNightShiftCharge
      congestionChargeByPerMin =
        fp.congestionChargePerMin >>= \congestionChargePerMin ->
          let duration = params.estimatedRideDuration <|> params.actualRideDuration
           in duration >>= \dur -> Just $ HighPrecMoney (realToFrac (fromIntegral dur / 60 * congestionChargePerMin))
      -- both components apply when both are set (a SurgeConfig row may output a
      -- multiplier AND a per-minute charge); historically per-min silently won
      congestionChargeResult = case (congestionChargeByPerMin, congestionChargeByMultiplier) of
        (Just perMin, Just byMultiplier) -> Just (perMin + byMultiplier)
        (perMin, byMultiplier) -> perMin <|> byMultiplier
      congestionChargeResultWithAddition = fromMaybe 0.0 congestionChargeResult + fp.additionalCongestionCharge
      finalCongestionCharge = capComponent CongestionChargeComponent $ fromMaybe 0.0 (params.estimatedCongestionCharge <|> Just congestionChargeResultWithAddition)
      insuranceChargeResult = capComponentMb InsuranceChargeComponent $ countInsuranceChargeForDistance fp.distanceUnit params.actualDistance fp.perDistanceUnitInsuranceCharge
      petChargesResult = capComponentMb PetChargeComponent params.petCharges
      driverAllowanceResult = capComponentMb DriverAllowanceComponent fp.driverAllowance
      airportConvenienceFeeResult = capComponentMb AirportConvenienceFeeComponent fp.airportConvenienceFee
      serviceChargeResult = capComponentMb ServiceChargeComponent fp.serviceCharge
      priorityChargesResult = capComponentMb PriorityChargeComponent fp.priorityCharges
      -- petCharges = if params.isPetRide then fp.petCharges else Nothing
      luggageCharge = capComponentMb LuggageChargeComponent $ case (fp.perLuggageCharge, params.numberOfLuggages) of
        (Just perLuggageCharge, Just numberOfLuggages) -> Just $ perLuggageCharge * fromIntegral numberOfLuggages
        _ -> Nothing
      returnFeeCharge = capComponentMb ReturnFeeChargeComponent $ case fp.returnFee of
        Just (DFP.ReturnFeeFixed fee) -> Just fee
        Just (DFP.ReturnFeePercentage p) -> Just $ partOfNightShiftCharge * fromRational (toRational p / 100)
        _ -> Nothing
      boothCharge = capComponentMb BoothChargeComponent $ case fp.boothCharges of
        Just (DFP.BoothChargeFixed fee) -> Just fee
        Just (DFP.BoothChargePercentage p) -> Just $ partOfNightShiftCharge * fromRational (toRational p / 100)
        _ -> Nothing

      schedulingChargeResult =
        capComponentMb SchedulingChargeComponent $
          if params.isScheduled
            then
              fp.schedulingCharge <&> \case
                DFP.ProgressiveSchedulingCharge percent -> fullRideCost * fromRational (toRational percent / 100)
                DFP.ConstantSchedulingCharge charge -> charge
            else Nothing
      fullRideCostN {-without govtCharges, platformFee, cardChargeOnFare and fixedCharge-} =
        fullRideCost
          + fromMaybe 0.0 resultNightShiftCharge
          + fromMaybe 0.0 resultWaitingCharge
          + fromMaybe 0.0 schedulingChargeResult
          + finalCongestionCharge ----------Needs to be changed to congestionChargeResult
          + fromMaybe 0.0 petChargesResult
          + fromMaybe 0.0 driverAllowanceResult
          + fromMaybe 0.0 airportConvenienceFeeResult
          + fromMaybe 0.0 serviceChargeResult
          + fromMaybe 0.0 priorityChargesResult
          + fromMaybe 0.0 insuranceChargeResult
          + notPartOfNightShiftCharge
      govtCharges = Nothing :: Maybe HighPrecMoney
      stopCharges =
        capComponentMb StopChargeComponent $ HighPrecMoney . ((toRational params.noOfStops) *) . toRational <$> fp.perStopCharge
      extraTimeFareInfo = capComponentMb RideExtraTimeFareComponent $ calculateExtraTimeFare params.estimatedRideDuration fp.rideExtraTimeChargeGracePeriod fp.perMinuteRideExtraTimeCharge params.actualRideDuration
      parkingChargeResult = capComponentMb ParkingChargeComponent fp.parkingCharge
      customerExtraFeeResult = capComponentMb CustomerExtraFeeComponent params.customerExtraFee
      customerCancellationDuesResult = capComponentMb CustomerCancellationChargeComponent params.customerCancellationDues
      carriedDuesFactor = (\gross capped -> if gross > 0 then capped.getHighPrecMoney / gross.getHighPrecMoney else 0) <$> params.customerCancellationDues <*> customerCancellationDuesResult
      scaleCarried mbTax = carriedDuesFactor >>= \factor -> mbTax >>= RideTax.scaleTaxBreakup params.currency factor
      carriedDuesTax = scaleCarried params.customerCancellationDuesTaxBreakup
      carriedDuesCommissionTax = scaleCarried params.customerCancellationDuesCommissionTaxBreakup
      tollChargesResult = if params.isManualTollCharge then addMaybes fp.tollCharges params.tollCharges else capComponentMb TollChargesComponent $ addMaybes fp.tollCharges (if isTollApplicableForTrip fp.vehicleServiceTier fp.tripCategory then params.tollCharges else Nothing)
      fullCompleteRideCost =
        {- without platformFee -}
        fullRideCostN
          + fromMaybe 0 govtCharges
      cardChargeOnFare = countCardChargeOnFare fullCompleteRideCost <$> (fp.cardCharge >>= (.perDistanceUnitMultiplier))
      businessDiscount = if params.shouldApplyBusinessDiscount then fp.businessDiscountPercentage >>= computeRideDiscount fareParametersDetails baseFare (Just finalCongestionCharge) resultNightShiftCharge stopCharges else Nothing
      personalDiscount = if params.shouldApplyPersonalDiscount then fp.personalDiscountPercentage >>= computeRideDiscount fareParametersDetails baseFare (Just finalCongestionCharge) resultNightShiftCharge stopCharges else Nothing
      fareParams =
        FareParameters
          { id,
            driverSelectedFare = params.driverSelectedFare,
            customerExtraFee = customerExtraFeeResult,
            negativeFareAdjustment = params.negativeFareAdjustment,
            shouldApplyBusinessDiscount = params.shouldApplyBusinessDiscount,
            shouldApplyPersonalDiscount = params.shouldApplyPersonalDiscount,
            serviceCharge = serviceChargeResult,
            parkingCharge = if params.isParkingFeeExempt && fromMaybe False fp.parkingFeeExemptionEnabled then Nothing else parkingChargeResult,
            baseFare = baseFare,
            petCharges = petChargesResult,
            driverAllowance = driverAllowanceResult,
            airportConvenienceFee = airportConvenienceFeeResult,
            priorityCharges = priorityChargesResult,
            congestionCharge = Just finalCongestionCharge,
            congestionChargeViaDp = congestionChargeByPerMin,
            stopCharges = stopCharges, --(\charges -> Just $ HighPrecMoney (toRational params.noOfStops * charges))=<< fp.perStopCharge,
            waitingCharge = resultWaitingCharge,
            nightShiftCharge = resultNightShiftCharge,
            rideExtraTimeFare = extraTimeFareInfo,
            nightShiftRateIfApplies =
              if nightShiftBuffer
                then Just $ realToFrac ((fromMaybe (HighPrecMoney 0.0) resultNightShiftCharge + partOfNightShiftCharge).getHighPrecMoney / partOfNightShiftCharge.getHighPrecMoney)
                else
                  if isNightShiftChargeIncluded
                    then getNightShiftRate nightShiftCharge
                    else Nothing, -- Temp fix :: have to fix properly
            fareParametersDetails = case fp.farePolicyDetails of
              DFP.ProgressiveDetails _ -> fareParametersDetails
              DFP.SlabsDetails det ->
                countPlatformFee -- Mb change platformFee from Nothing to proper value
                  fullCompleteRideCost
                  (DFP.findFPSlabsDetailsSlabByDistance (fromMaybe 0 params.actualDistance) det.slabs & (.platformFeeInfo))
                  params.currency
                  fareParametersDetails
              DFP.RentalDetails _ -> fareParametersDetails
              DFP.InterCityDetails _ -> fareParametersDetails
              DFP.AmbulanceDetails det ->
                countPlatformFee
                  fullCompleteRideCost
                  (DFP.findFPAmbulanceDetailsSlabByAge (fromMaybe 0 params.vehicleAge) det.slabs & (.platformFeeInfo))
                  params.currency
                  fareParametersDetails,
            customerCancellationDues = customerCancellationDuesResult,
            tollCharges = tollChargesResult,
            govtCharges = govtCharges,
            insuranceCharge = insuranceChargeResult,
            luggageCharge = luggageCharge,
            returnFeeCharge = returnFeeCharge,
            boothCharge = boothCharge,
            schedulingCharge = schedulingChargeResult,
            cardCharge =
              Just
                DFParams.CardCharge
                  { onFare = cardChargeOnFare,
                    fixed = fp.cardCharge >>= (.fixed)
                  },
            updatedAt = now,
            currency = params.currency,
            platformFee = fp.platformFee,
            sgst = fp.sgst,
            cgst = fp.cgst,
            tdsAmount = Nothing,
            tdsRate = Nothing,
            tdsProcessedAt = Nothing,
            platformFeeChargesBy = fp.platformFeeChargesBy,
            merchantId = Just params.farePolicy.merchantId,
            merchantOperatingCityId = params.merchantOperatingCityId,
            conditionalCharges = filter (\addCharges -> DAC.isFareComponent addCharges.chargeCategory && maybe True (\chargesCategories -> addCharges.chargeCategory `elem` chargesCategories) params.mbAdditonalChargeCategories) params.farePolicy.conditionalCharges,
            customerGateFeeItems = Nothing,
            driverCancellationNotAllowed = fp.driverCancellationNotAllowed,
            businessDiscount = businessDiscount,
            personalDiscount = personalDiscount,
            paymentProcessingFee = Nothing,
            paymentProcessingFeeTax = Nothing,
            isVatTaxType = Nothing,
            discountApplicableRideFareTaxExclusive = Nothing,
            discountApplicableRideFareTax = Nothing,
            nonDiscountApplicableRideFareTaxExclusive = Nothing,
            nonDiscountApplicableRideFareTax = Nothing,
            tollFareTaxExclusive = Nothing,
            tollFareTax = Nothing,
            cancellationFeeTaxExclusive = Nothing,
            cancellationTax = Nothing,
            parkingChargeTaxExclusive = Nothing,
            parkingChargeTax = Nothing,
            fareSettlementType = params.fareSettlementType,
            negotiatedFareDelta = params.negotiatedFareDelta,
            addOnCharges = params.addOnCharges,
            -- Filled in by 'calculateFareParameters' once the full params exist.
            bufferedFare = Nothing,
            rideTaxCgst = Nothing,
            rideTaxSgst = Nothing,
            rideTaxIgst = Nothing,
            parkingTaxCgst = Nothing,
            parkingTaxSgst = Nothing,
            parkingTaxIgst = Nothing,
            tollTaxCgst = Nothing,
            tollTaxSgst = Nothing,
            tollTaxIgst = Nothing,
            airportEntryFee = Nothing,
            airportEntryFeeTax = Nothing,
            airportEntryFeeTaxCgst = Nothing,
            airportEntryFeeTaxSgst = Nothing,
            airportEntryFeeTaxIgst = Nothing,
            paymentChargeTaxCgst = Nothing,
            paymentChargeTaxSgst = Nothing,
            paymentChargeTaxIgst = Nothing,
            customerCancellationDuesTaxExclusive = (\dues -> dues - taxTotal carriedDuesTax) <$> customerCancellationDuesResult,
            customerCancellationDuesTax = (.total) <$> carriedDuesTax,
            cancellationTaxCgst = carriedDuesTax >>= (.cgst),
            cancellationTaxSgst = carriedDuesTax >>= (.sgst),
            cancellationTaxIgst = carriedDuesTax >>= (.igst),
            customerCancellationDuesCommission = (\factor carriedCommission -> roundAmountByCurrency' params.currency (carriedCommission * fromRational factor)) <$> carriedDuesFactor <*> params.customerCancellationDuesCommission,
            customerCancellationDuesCommissionTax = (.total) <$> carriedDuesCommissionTax,
            customerCancellationDuesCommissionTaxCgst = carriedDuesCommissionTax >>= (.cgst),
            customerCancellationDuesCommissionTaxSgst = carriedDuesCommissionTax >>= (.sgst),
            customerCancellationDuesCommissionTaxIgst = carriedDuesCommissionTax >>= (.igst)
          }
  -- The FCBuffer pass re-runs this handler only to price the buffered ceiling;
  -- skip its info logs so a capped estimate doesn't emit the slab debug trail
  -- and the full params dump twice per calculation.
  unless (params.computationPhase == FCBuffer) $ do
    KP.forM_ debugLogs $ logTagInfo ("FareCalculator:FarePolicyId:" <> show fp.id.getId)
    logTagInfo "FareCalculator" $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
  where
    estimateComponentMap = maybe Map.empty buildComponentMap params.mbEstimateFareParams
    capComponent component value =
      case (params.computationPhase, params.mbCapConfig) of
        (FCRecompute, Just capConfig) -> capByStrategy (lookupCapStrategy capConfig component) (componentAmount estimateComponentMap component) value
        (FCBuffer, Just capConfig) -> maybe value (\strategy -> value + capAllowance strategy value) (lookupCapStrategy capConfig component)
        _ -> value
    capComponentMb component = fmap (capComponent component)
    processFarePolicyDetails = \case
      DFP.ProgressiveDetails det -> processFPProgressiveDetails det
      DFP.SlabsDetails det -> processFPSlabsDetailsSlab $ DFP.findFPSlabsDetailsSlabByDistance (fromMaybe 0 params.actualDistance) det.slabs
      DFP.RentalDetails det -> processFPRentalDetails det
      DFP.InterCityDetails det -> processFPInterCityDetails det
      DFP.AmbulanceDetails det -> processFPAmbulanceDetailsSlab $ DFP.findFPAmbulanceDetailsSlabByAge (fromMaybe 0 params.vehicleAge) det.slabs

    processFPAmbulanceDetailsSlab DFP.FPAmbulanceDetailsSlab {..} = do
      let mbExtraDistance = (fromMaybe 0 params.actualDistance) - baseDistance & (\dist -> if dist > 0 then Just dist else Nothing)
          distanceInKm = (fromIntegral $ fromMaybe 0 mbExtraDistance) / 1000
          distBasedFare = HighPrecMoney $ perKmRate.getHighPrecMoney * distanceInKm
      ( [],
        baseFare,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.AmbulanceDetails
          DFParams.FParamsAmbulanceDetails
            { platformFee = Nothing,
              sgst = Nothing,
              cgst = Nothing,
              vat = Nothing,
              currency = currency,
              distBasedFare = capComponent AmbulanceDistBasedFareComponent distBasedFare
            }
        )

    processFPInterCityDetails DFP.FPInterCityDetails {..} = do
      let estimatedDuration = maybe 0 (.getSeconds) params.estimatedRideDuration
          estimatedDurationInMins = estimatedDuration `div` 60
          actualDuration = maybe estimatedDuration (.getSeconds) params.actualRideDuration
          perDayMaxAllowanceInMins' = case perDayMaxAllowanceInMins of
            Just allowance -> allowance.getMinutes
            Nothing -> 840
          allowanceMins' = maybe 0 (\rt -> (calculateAllowanceMins (fromMaybe 19800 params.timeDiffFromUtc) perDayMaxAllowanceInMins' params.rideTime rt) - estimatedDurationInMins) params.returnTime
          allowanceMins = if params.roundTrip then max defaultWaitTimeAtDestination.getMinutes allowanceMins' else 0
          reservedTime = (interCityReservedTime params.rideTime params.returnTime (Seconds estimatedDuration)).getSeconds
          extraMins = max 0 (actualDuration - reservedTime) `div` 60
          extraTimeFare = HighPrecMoney $ toRational extraMins * perExtraMinRate.getHighPrecMoney
          fareByTime = HighPrecMoney $ (toRational (estimatedDurationInMins + allowanceMins) / 60) * perHourCharge.getHighPrecMoney

      let perKmRate = if params.roundTrip then perKmRateRoundTrip else perKmRateOneWay
          estimatedDistance = maybe 0 (.getMeters) params.estimatedDistance
          estimatedDistanceInKm = estimatedDistance `div` 1000
          actualDistance = (.getMeters) <$> params.actualDistance
          actualDistanceInKm = fromMaybe 0 actualDistance `div` 1000
          extraDist = max 0 (actualDistanceInKm - estimatedDistanceInKm)
          extraDistanceFare = HighPrecMoney $ toRational extraDist * perExtraKmRate.getHighPrecMoney
          extraTimeSpent = max 0 ((diffUTCTime (fromMaybe params.rideTime params.returnTime) params.rideTime) - intToNominalDiffTime estimatedDuration) / 60
          extraHoursSpent = max 0 (realToFrac extraTimeSpent - fromIntegral (defaultWaitTimeAtDestination.getMinutes)) / 60.0 :: Double
          fareByDist = HighPrecMoney $ toRational ((extraHoursSpent * fromIntegral kmPerPlannedExtraHour.getKilometers) + fromIntegral estimatedDistanceInKm) * fromRational (perKmRate.getHighPrecMoney)

      let distPercent = case (actualDistance, estimatedDistance) of
            (Just ad, ed) -> if ed == 0 then 1 else min 1 (fromIntegral ad / (fromIntegral ed :: Double))
            _ -> 1
          timePercent = if estimatedDuration == 0 then 1 else min 1 (fromIntegral actualDuration / (fromIntegral estimatedDuration :: Double))
          pricingSlab = DFP.findFPInterCityDetailsByTimeAndDistancePercentage (round $ timePercent * 100) (round $ distPercent * 100) pricingSlabs
          distTimePercentApplied = (if pricingSlab.includeActualTimePercentage then timePercent else 0) + (if pricingSlab.includeActualDistPercentage then distPercent else 0)
          baseFare_ = HighPrecMoney (toRational (min 1 (distTimePercentApplied + (fromIntegral pricingSlab.farePercentage / 100 :: Double))) * baseFare.getHighPrecMoney)
          fareByTime_ = HighPrecMoney (toRational (min 1 (distTimePercentApplied + (fromIntegral pricingSlab.farePercentage / 100 :: Double))) * fareByTime.getHighPrecMoney)
          fareByDist_ = HighPrecMoney (toRational (min 1 (distTimePercentApplied + (fromIntegral pricingSlab.farePercentage / 100 :: Double))) * fareByDist.getHighPrecMoney)

      ( [],
        baseFare_,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.InterCityDetails $
          DFParams.FParamsInterCityDetails
            { timeFare = capComponent TimeFareComponent fareByTime_,
              distanceFare = capComponent DistanceFareComponent fareByDist_,
              pickupCharge = capComponent PickupChargeComponent deadKmFare,
              extraDistanceFare = capComponent ExtraDistanceFareComponent extraDistanceFare,
              extraTimeFare = capComponent ExtraTimeFareComponent extraTimeFare,
              stateEntryPermitCharges = capComponentMb StateEntryPermitChargesComponent stateEntryPermitCharges,
              currency = params.currency,
              ..
            }
        )

    processFPRentalDetails DFP.FPRentalDetails {..} = do
      let estimatedDuration = maybe 0 (.getSeconds) params.estimatedRideDuration
          actualDuration = maybe estimatedDuration (.getSeconds) params.actualRideDuration
          actualRideDurationInHr = actualDuration `div` 3600
          estimatedDurationInHr = estimatedDuration `div` 3600
          extraMins = max 0 (actualDuration - estimatedDuration) `div` 60
          fareByTime = HighPrecMoney $ toRational extraMins * perExtraMinRate.getHighPrecMoney

      let estimatedDistanceM = (.getMeters) <$> params.estimatedDistance
          plannedDistanceM = max (estimatedDurationInHr * includedKmPerHr.getKilometers * 1000) (fromMaybe 0 estimatedDistanceM)
          actualDistanceM = (.getMeters) <$> params.actualDistance
          extraDistM = max 0 (fromMaybe 0 actualDistanceM - plannedDistanceM)
          distanceBuffer = DFP.findFPRentalDetailsByDuration actualRideDurationInHr distanceBuffers
          fareByDist = if extraDistM > distanceBuffer.bufferMeters then HighPrecMoney (toRational (fromIntegral extraDistM / 1000 :: Double) * perExtraKmRate.getHighPrecMoney) else 0

      let extraPlannedKm = max 0 ((plannedDistanceM `div` 1000) - (estimatedDurationInHr * includedKmPerHr.getKilometers))
          extraPlannedKmFare = toRational extraPlannedKm * plannedPerKmRate.getHighPrecMoney
          potentialBaseFare = toRational estimatedDurationInHr * perHourCharge.getHighPrecMoney + extraPlannedKmFare

      let distPercent = case (actualDistanceM, estimatedDistanceM) of
            (Just ad, Just ed) -> if ed == 0 then 1 else min 1 (fromIntegral ad / (fromIntegral ed :: Double))
            _ -> 1
          timePercent = if estimatedDuration == 0 then 1 else min 1 (fromIntegral actualDuration / (fromIntegral estimatedDuration :: Double))
          pricingSlab = DFP.findFPRentalDetailsByTimeAndDistancePercentage (round $ timePercent * 100) (round $ distPercent * 100) pricingSlabs
          distTimePercentApplied = (if pricingSlab.includeActualTimePercentage then timePercent else 0) + (if pricingSlab.includeActualDistPercentage then distPercent else 0)
          baseFare_ = HighPrecMoney (toRational (min 1 (distTimePercentApplied + (fromIntegral pricingSlab.farePercentage / 100 :: Double))) * potentialBaseFare)

      ( [],
        baseFare_,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.RentalDetails $
          DFParams.FParamsRentalDetails
            { timeBasedFare = capComponent TimeBasedFareComponent fareByTime,
              distBasedFare = capComponent DistBasedFareComponent fareByDist,
              deadKmFare = capComponent DeadKmFareComponent deadKmFare,
              extraDistance = Meters extraDistM,
              extraDuration = Seconds $ extraMins * 60,
              currency = params.currency,
              distanceUnit = params.distanceUnit,
              ..
            }
        )

    processFPProgressiveDetails DFP.FPProgressiveDetails {..} = do
      let mbExtraDistance =
            (fromMaybe 0 params.actualDistance) - baseDistance
              & (\dist -> if dist > 0 then Just dist else Nothing)
          mbEstimatedRideDurationInMins = ceiling . (fromIntegral @_ @Double) . (`div` 60) . (.getSeconds) <$> params.estimatedRideDuration
          -- what duration perMinRateSections is priced on, chosen per fare policy:
          -- TotalDuration (default) - full traffic-aware estimated duration;
          -- TrafficDelayDuration - only the delay over the traffic-free (static)
          -- duration, so sections express a buffer (zero-rate first section) and
          -- per-minute delay rates. Missing static duration => no delay fare.
          mbPerMinRateChargeableMins = case fromMaybe DFP.TotalDuration perMinRateDurationBasis of
            DFP.TotalDuration -> mbEstimatedRideDurationInMins
            DFP.TrafficDelayDuration -> do
              estimatedDuration <- params.estimatedRideDuration
              staticDuration <- params.estimatedRideStaticDuration
              let delaySecs = max 0 (estimatedDuration.getSeconds - staticDuration.getSeconds)
              pure $ ceiling ((fromIntegral @_ @Double) (delaySecs `div` 60))
          (extraKmFare, baseFareDepreciation) = maybe (HighPrecMoney 0.0, HighPrecMoney 0.0) (processFPProgressiveDetailsPerExtraKmFare perExtraKmRateSections) mbExtraDistance
          (mbRideDurationFare, debugLogs) = maybe (Nothing, []) (processFPProgressiveDetailsPerRideDurationMinFare perMinRateSections) mbPerMinRateChargeableMins
      ( debugLogs,
        baseFare + baseFareDepreciation,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.ProgressiveDetails $
          DFParams.FParamsProgressiveDetails
            { extraKmFare = capComponentMb ExtraKmFareComponent $ if extraKmFare == 0.0 then Nothing else Just extraKmFare,
              rideDurationFare = capComponentMb RideDurationFareComponent mbRideDurationFare,
              deadKmFare = capComponent DeadKmFareComponent deadKmFare,
              ..
            }
        )

    processFPProgressiveDetailsPerExtraKmFare perExtraKmRateSections (extraDistance :: Meters) = do
      let sortedPerExtraKmFareSections = NE.sortBy (comparing (.startDistance)) perExtraKmRateSections
      processFPProgressiveDetailsPerExtraKmFare' sortedPerExtraKmFareSections extraDistance
      where
        processFPProgressiveDetailsPerExtraKmFare' _ 0 = (0 :: HighPrecMoney, 0 :: HighPrecMoney)
        processFPProgressiveDetailsPerExtraKmFare' sortedPerExtraKmFareSectionsLeft (extraDistanceLeft :: Meters) =
          case sortedPerExtraKmFareSectionsLeft of
            aSection :| [] -> (HighPrecMoney $ (toRational extraDistanceLeft * (getPerExtraMRate aSection.perExtraKmRate).getHighPrecMoney), aSection.baseFareDepreciation)
            aSection :| bSection : leftSections -> do
              let sectionDistance = bSection.startDistance - aSection.startDistance
                  extraDistanceWithinSection = min sectionDistance extraDistanceLeft
                  (extraFare, depreciation) = processFPProgressiveDetailsPerExtraKmFare' (bSection :| leftSections) (extraDistanceLeft - extraDistanceWithinSection)
              (HighPrecMoney ((toRational extraDistanceWithinSection * (getPerExtraMRate aSection.perExtraKmRate).getHighPrecMoney) + extraFare.getHighPrecMoney), aSection.baseFareDepreciation + depreciation)
        getPerExtraMRate perExtraKmRate = perExtraKmRate / 1000

    processFPProgressiveDetailsPerRideDurationMinFare mbPerMinRateSections rideDurationInMins =
      case mbPerMinRateSections of
        Nothing -> (Nothing, ["No per min rate sections configured"])
        Just perMinRateSections -> do
          let sortedPerMinFareSections = NE.sortBy (comparing (.rideDurationInMin)) perMinRateSections
              fpDebugLogStr = "Sorted per min rate sections: " +|| NE.toList sortedPerMinFareSections ||+ " for ride duration (in mins): " +|| rideDurationInMins ||+ ""

          let rideDurationFare = HighPrecMoney $ processFPPDPerMinFare rideDurationInMins sortedPerMinFareSections
              rideFareDebugLogStr = "Ride duration fare: " +|| rideDurationFare ||+ ""

          (Just rideDurationFare, [fpDebugLogStr, rideFareDebugLogStr])
      where
        {- Sections for e.g.: (0, 5], (5, 15], (15, 30] -}
        processFPPDPerMinFare 0 _ = 0.0 :: Rational
        processFPPDPerMinFare !remRideDuration (aSection :| []) = toRational remRideDuration * aSection.perMinRate.amount.getHighPrecMoney
        processFPPDPerMinFare !remRideDuration (aSection :| bSection : remSections) =
          let sectionDuration = bSection.rideDurationInMin - aSection.rideDurationInMin
              rideDurationWithinSection = min sectionDuration remRideDuration
              remFare = processFPPDPerMinFare (remRideDuration - rideDurationWithinSection) (bSection :| remSections)
           in toRational rideDurationWithinSection * aSection.perMinRate.amount.getHighPrecMoney + remFare

    processFPSlabsDetailsSlab DFP.FPSlabsDetailsSlab {..} = do
      ( [],
        baseFare,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.SlabDetails
          DFParams.FParamsSlabDetails
            { platformFee = Nothing, -- Nothing for now, can be counted only after everything else
              sgst = Nothing,
              cgst = Nothing,
              vat = Nothing,
              currency
            }
        )

    countNightShiftCharge :: HighPrecMoney -> NightShiftCharge -> HighPrecMoney
    countNightShiftCharge fullRideCost nightShiftCharge = do
      case nightShiftCharge of
        ProgressiveNightShiftCharge charge -> HighPrecMoney (fullRideCost.getHighPrecMoney * toRational charge) - fullRideCost
        ConstantNightShiftCharge charge -> toHighPrecMoney charge -- FIXME change ConstantWaitingCharge Money to HighPrecMoney
    getNightShiftRate nightShiftCharge = do
      -- Temp fix :: have to fix properly
      case nightShiftCharge of
        Just (ProgressiveNightShiftCharge charge) -> (Just . realToFrac) charge
        _ -> Nothing

    countWaitingCharge :: WaitingChargeInfo -> Maybe HighPrecMoney
    countWaitingCharge waitingChargeInfo = do
      let waitingTimeMinusFreeWatingTime = max 0 (fromMaybe (Minutes 0) (params.waitingTime <&> (\wt -> (-) wt waitingChargeInfo.freeWaitingTime)))
      let totalStopWaitingTimeMinunFreeWaitingTime = sum $ params.stopWaitingTimes <&> (\wt -> max ((-) wt waitingChargeInfo.freeWaitingTime) 0)
      let chargedWaitingTime = waitingTimeMinusFreeWatingTime + totalStopWaitingTimeMinunFreeWaitingTime
      let mbChargedWaitingTime = if chargedWaitingTime == 0 then Nothing else Just chargedWaitingTime
      case waitingChargeInfo.waitingCharge of
        PerMinuteWaitingCharge charge -> (\waitingTime -> HighPrecMoney $ toRational waitingTime * charge.getHighPrecMoney) <$> mbChargedWaitingTime
        ConstantWaitingCharge charge -> Just $ toHighPrecMoney charge -- Always charged, freeWaitingTime doesn't make sense in this case
    countPlatformFee :: HighPrecMoney -> Maybe PlatformFeeInfo -> Currency -> FareParametersDetails -> FareParametersDetails
    countPlatformFee fullCompleteRideCost platformFeeInfo currency = \case
      (DFParams.ProgressiveDetails det) -> DFParams.ProgressiveDetails det -- should be impossible anyway
      (DFParams.RentalDetails det) -> DFParams.RentalDetails det
      (DFParams.InterCityDetails det) -> DFParams.InterCityDetails det
      (DFParams.SlabDetails det) ->
        DFParams.SlabDetails $ maybe (FParamsSlabDetails Nothing Nothing Nothing Nothing det.currency) countPlatformFeeMath platformFeeInfo
      (DFParams.AmbulanceDetails det) ->
        DFParams.AmbulanceDetails $ maybe (FParamsAmbulanceDetails Nothing Nothing Nothing Nothing det.distBasedFare det.currency) (countPlatformFeeAmbulance det) platformFeeInfo
      where
        countPlatformFeeMath platformFeeInfo' = do
          let (platformFee, cgst, sgst, vat) = getPlatformFee platformFeeInfo'
          FParamsSlabDetails {..}
        countPlatformFeeAmbulance det platformFeeInfo' = do
          let (platformFee, cgst, sgst, vat) = getPlatformFee platformFeeInfo'
          FParamsAmbulanceDetails {distBasedFare = det.distBasedFare, currency = det.currency, ..}
        getPlatformFee platformFeeInfo' = do
          let baseFee =
                capComponent PlatformFeeComponent $ case platformFeeInfo'.platformFeeCharge of
                  ProgressivePlatformFee charge -> fullCompleteRideCost * charge
                  ConstantPlatformFee charge -> charge
              cgst = Just . HighPrecMoney . toRational $ platformFeeInfo'.cgst * realToFrac baseFee
              sgst = Just . HighPrecMoney . toRational $ platformFeeInfo'.sgst * realToFrac baseFee
              vat = (\rate -> HighPrecMoney . toRational $ rate * realToFrac baseFee) <$> platformFeeInfo'.vat
          (Just baseFee, cgst, sgst, vat)

    calculateExtraTimeFare :: Maybe Seconds -> Maybe Seconds -> Maybe HighPrecMoney -> Maybe Seconds -> Maybe HighPrecMoney
    calculateExtraTimeFare (Just estimatedDuration) rideExtraTimeChargeGracePeriod (Just perMinuteRideExtraTimeCharge) (Just actualRideDuration) = do
      let actualRideDurationInMinutes = getMinutes $ secondsToMinutes actualRideDuration
      let estimatedDurationInMinutes = getMinutes $ secondsToMinutes estimatedDuration
      let rideExtraTimeChargeGracePeriodInMinutes = fromMaybe 0 (getMinutes <$> secondsToMinutes <$> rideExtraTimeChargeGracePeriod)
      let rideDurationDifference = max 0 (actualRideDurationInMinutes - (estimatedDurationInMinutes + rideExtraTimeChargeGracePeriodInMinutes))
      let extraTimeFare = HighPrecMoney $ (* perMinuteRideExtraTimeCharge.getHighPrecMoney) $ toRational rideDurationDifference
      if extraTimeFare.getHighPrecMoney > 0 then Just extraTimeFare else Nothing
    calculateExtraTimeFare _ _ _ _ = Nothing

    countInsuranceChargeForDistance :: DistanceUnit -> Maybe Meters -> Maybe HighPrecMoney -> Maybe HighPrecMoney
    countInsuranceChargeForDistance dUnit mbDistance mbChargePerUnit =
      liftM2 (,) mbDistance mbChargePerUnit
        <&> \(distanceInMtrs, chargePerUnit) ->
          let distance = convertMetersToDistance dUnit distanceInMtrs
           in HighPrecMoney $ distance.value.getHighPrecDistance * chargePerUnit.getHighPrecMoney
    countCardChargeOnFare :: HighPrecMoney -> Double -> HighPrecMoney
    countCardChargeOnFare fullCompleteRideCost cardCharge =
      HighPrecMoney (fullCompleteRideCost.getHighPrecMoney * toRational (max 1 cardCharge)) - fullCompleteRideCost

----------- Discount includes Base Fare, Night Shift Charge, Congestion Charge, pickup charges, Duration Fare, stop charges and distance Fare
computeRideDiscount :: DFParams.FareParametersDetails -> HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Double -> Maybe HighPrecMoney
computeRideDiscount fareParametersDetails baseFare congestionCharge nightShiftCharge stopCharges discountPercentage = do
  let (partOfNightShiftCharge, notPartOfNightShiftCharge, _) = countFullFareOfParamsDetails fareParametersDetails
  let fullRideCost = baseFare + partOfNightShiftCharge + notPartOfNightShiftCharge + fromMaybe 0.0 congestionCharge + fromMaybe 0.0 nightShiftCharge + fromMaybe 0.0 stopCharges
  return $ HighPrecMoney (fullRideCost.getHighPrecMoney * toRational discountPercentage / 100)

countFullFareOfParamsDetails :: DFParams.FareParametersDetails -> (HighPrecMoney, HighPrecMoney, HighPrecMoney)
countFullFareOfParamsDetails = \case
  DFParams.ProgressiveDetails det -> (fromMaybe 0.0 det.extraKmFare, det.deadKmFare + fromMaybe 0.0 det.rideDurationFare, 0.0) -- (partOfNightShiftCharge, notPartOfNightShiftCharge)
  DFParams.SlabDetails det -> (0.0, 0.0, fromMaybe 0.0 det.platformFee + fromMaybe 0.0 det.sgst + fromMaybe 0.0 det.cgst + fromMaybe 0.0 det.vat)
  DFParams.RentalDetails det -> (det.distBasedFare + det.timeBasedFare, det.deadKmFare, 0.0)
  DFParams.InterCityDetails det -> (det.distanceFare + det.timeFare + det.extraDistanceFare + det.extraTimeFare, det.pickupCharge + fromMaybe 0.0 det.stateEntryPermitCharges, 0.0)
  DFParams.AmbulanceDetails det -> (det.distBasedFare, 0.0, fromMaybe 0.0 det.platformFee + fromMaybe 0.0 det.sgst + fromMaybe 0.0 det.cgst + fromMaybe 0.0 det.vat)

addMaybes :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybes Nothing y = y
addMaybes x Nothing = x
addMaybes (Just x) (Just y) = Just (x + y)

calNightShiftCharge :: Maybe HighPrecMoney -> UTCTime -> Maybe NightShiftBounds -> Seconds -> Maybe HighPrecMoney
calNightShiftCharge resultFullNightShiftCharge rideTime (Just nightShiftBounds) duration = do
  let resultFullNightShiftCharge' = fromMaybe 0 resultFullNightShiftCharge
  let rideStartTime = secondsFromTimeOfDay $ localTimeOfDay $ utcToLocalTime timeZoneIST rideTime
  let nightShiftStart = secondsFromTimeOfDay nightShiftBounds.nightShiftStart
  Just $
    toHighPrecMoney (int2Double (duration - (nightShiftStart - rideStartTime)).getSeconds / int2Double duration.getSeconds) * resultFullNightShiftCharge'
calNightShiftCharge _ _ _ _ = Nothing

isNightShift ::
  NightShiftBounds ->
  UTCTime ->
  Bool
isNightShift nightShiftBounds time = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST time
  let nightShiftStart = nightShiftBounds.nightShiftStart
  let nightShiftEnd = nightShiftBounds.nightShiftEnd
  isTimeWithinBounds nightShiftStart nightShiftEnd timeOfDay

isNightShiftWithPickupBuffer :: NightShiftBounds -> UTCTime -> UTCTime -> Bool
isNightShiftWithPickupBuffer nightShiftBounds time endTime = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST time
  let endTimeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST endTime
  let nightShiftStart = nightShiftBounds.nightShiftStart
  let nightShiftEnd = nightShiftBounds.nightShiftEnd
  if nightShiftStart >= nightShiftEnd
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      if endTimeOfDay < midnightBeforeTimeleap
        then do
          timeOfDay < nightShiftStart && endTimeOfDay > nightShiftStart
        else timeOfDay < nightShiftStart && timeOfDay < midnightBeforeTimeleap
    else --  (midnight <= timeOfDay && endTimeOfDay < nightShiftEnd)  || (midnight > timeOfDay && timeOfDay < nightShiftEnd)
      timeOfDay < nightShiftStart && timeOfDay < nightShiftEnd && endTimeOfDay > nightShiftStart

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
isTimeWithinBounds startTime endTime time =
  if startTime >= endTime
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      (startTime < time && time < midnightBeforeTimeleap) || (midnight <= time && time < endTime)
    else startTime < time && time < endTime

isNightAllowanceApplicable :: Maybe NightShiftBounds -> UTCTime -> UTCTime -> Seconds -> Bool
isNightAllowanceApplicable nightShiftBounds tripStartTime tripEndTime timeDiffFromUtc = do
  (diffUTCTime tripEndTime tripStartTime >= 86400)
    || ( do
           let localRideEndDate = utctDay $ addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) tripEndTime
               localTripStartTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) tripStartTime
               localRideEndTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) tripEndTime
           case nightShiftBounds of
             Nothing -> False
             Just bounds -> do
               let nightShiftStartTime = timeOfDayToDiffTime bounds.nightShiftStart
                   nightShiftEndTime = timeOfDayToDiffTime bounds.nightShiftEnd
               if nightShiftStartTime <= 6 * 60 * 60 -- NS starting and ending on same date
                 then isNightShiftOverlap localRideEndDate nightShiftStartTime nightShiftEndTime localTripStartTime localRideEndTime 0 0
                 else isNightShiftOverlap localRideEndDate nightShiftStartTime nightShiftEndTime localTripStartTime localRideEndTime (-1) 0 || isNightShiftOverlap localRideEndDate nightShiftStartTime nightShiftEndTime localRideEndTime localRideEndTime 0 1
       )

isNightShiftOverlap :: Day -> DiffTime -> DiffTime -> UTCTime -> UTCTime -> Integer -> Integer -> Bool
isNightShiftOverlap rideEndDate nightShiftStartTime nightShiftEndTime localTripStartTime localRideEndTime startAdd endAdd = do
  let curNightShiftStartTs = UTCTime (addDays startAdd rideEndDate) nightShiftStartTime
      curNightShiftEndTs = UTCTime (addDays endAdd rideEndDate) nightShiftEndTime
      curMxStart = max curNightShiftStartTs localTripStartTime
      curMnEnd = min curNightShiftEndTs localRideEndTime
  curMnEnd >= curMxStart

timeOfDayToDiffTime :: TimeOfDay -> DiffTime
timeOfDayToDiffTime (TimeOfDay h m s) = secondsToDiffTime $ fromIntegral (h * 3600 + m * 60 + floor s)

-- Converts UTC time to Local(IST) time
utcToIst :: Minutes -> UTCTime -> LocalTime
utcToIst timeZoneDiff = utcToLocalTime (minutesToTimeZone timeZoneDiff.getMinutes) -- IST is UTC + 5:30

minuteOfDay :: TimeOfDay -> Int
minuteOfDay (TimeOfDay hour minute _) = hour * 60 + minute

-- Calculates hours for a partial day
hoursInDay :: UTCTime -> UTCTime -> LocalTime -> LocalTime -> Day -> Int
hoursInDay startUtc endUtc start end day
  | startDay == endDay = if startDay == day then diffMins else 0
  | otherwise =
    if day == startDay
      then 1440 - minuteOfDay startTod
      else
        if day == endDay
          then minuteOfDay endTod
          else 1440
  where
    startDay = localDay start
    endDay = localDay end
    startTod = localTimeOfDay start
    endTod = localTimeOfDay end
    diffSeconds = nominalDiffTimeToSeconds $ diffUTCTime endUtc startUtc
    diffMins = diffSeconds.getSeconds `div` 60

interCityReservedTime :: UTCTime -> Maybe UTCTime -> Seconds -> Seconds
interCityReservedTime rideTime mbReturnTime estimatedDuration =
  maybe estimatedDuration (\returnTime -> max estimatedDuration (nominalDiffTimeToSeconds $ diffUTCTime returnTime rideTime)) mbReturnTime

-- Main function to calculate total hours with 14-hour daily cap
calculateAllowanceMins :: Seconds -> Int -> UTCTime -> UTCTime -> Int
calculateAllowanceMins timeDiffFromUtc perDayMaxAllowanceInMins startTime endTime =
  sum $ map (min perDayMaxAllowanceInMins . hoursInDay startTime endTime startIST endIST) [startDay .. endDay]
  where
    startIST = utcToIst (secondsToMinutes timeDiffFromUtc) startTime
    endIST = utcToIst (secondsToMinutes timeDiffFromUtc) endTime
    startDay = localDay startIST
    endDay = localDay endIST

-- | Compute total GST rate as a fractional Double from TaxConfig GstBreakup.
--   E.g. cgstPercentage=9, sgstPercentage=9 → total = 0.18
--   Returns Nothing if the total percentage is 0 or all sub-rates are Nothing.
computeTotalGstRate :: DTC.GstBreakup -> Maybe Double
computeTotalGstRate gstBreakup =
  let cgst = maybe 0 (fromRational . (.getHighPrecMoney)) gstBreakup.cgstPercentage
      sgst = maybe 0 (fromRational . (.getHighPrecMoney)) gstBreakup.sgstPercentage
      igst = maybe 0 (fromRational . (.getHighPrecMoney)) gstBreakup.igstPercentage
      total = cgst + sgst + igst
   in if total > 0 then Just total else Nothing

-- ==========================================================================
-- Configurable charges (formerly FareCalculatorV2)
-- ==========================================================================

-- | Map of fare components to their monetary values
-- Used to compute charges on specific components (e.g., VAT on RideFare + CongestionChargeComponent)
type ComponentMap = Map.Map FareChargeComponent HighPrecMoney

-- | Parsed charge value - either a percentage or fixed amount
data ParsedCodeValue
  = ParsedPercentage Rational -- e.g., "14%" -> 0.14
  | ParsedFixed HighPrecMoney -- e.g., "50" -> 50.0

calculateFareParameters ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, BeamFlow m r) =>
  CalculateFareParametersParams ->
  m FareParameters
calculateFareParameters params = do
  merchantOpCityId <- params.merchantOperatingCityId & fromMaybeM (InternalError "MerchantOperatingCityId is required to apply ride tax")
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let rideTax = RideTax.resolveTaxRates transporterConfig (Just params.farePolicy) params.paymentChargeDetails
  fareWithGrossUp <- runFarePipeline rideTax params
  bufferedFare <- case (params.computationPhase, params.farePolicy.fareRecomputeCapConfig) of
    (FCEstimate, Just capConfig) -> do
      bufferedParams <- runFarePipeline rideTax params {computationPhase = FCBuffer, mbCapConfig = Just capConfig}
      pure $ Just (fareSum bufferedParams Nothing)
    _ -> pure Nothing
  pure fareWithGrossUp {bufferedFare = bufferedFare}
  where
    runFarePipeline rideTax prms = do
      baseFareParams <- calculateFareParametersHandler prms
      fareWithGateItems <- applyGateCustomerFeeItems prms baseFareParams
      fareWithAirport <- applyAirportEntryFee prms fareWithGateItems
      pure $ applyPaymentChargeDetails prms.paymentChargeDetails prms.buyerOfferDiscount (applyRideTax rideTax fareWithAirport)

fareParamsBases :: FareParameters -> FB.RideTaxRates HighPrecMoney
fareParamsBases fp =
  FB.RideTaxRates
    { rideFare = (fareParamsAmounts Nothing fp).rideBase,
      toll = fromMaybe 0 fp.tollCharges,
      parking = fromMaybe 0 fp.parkingCharge,
      airportEntryFee = fromMaybe 0 fp.airportEntryFee + customerGateFeeItemsSum fp,
      cancellationFee = 0,
      paymentCharge = 0
    }

applyRideTax :: Tax.RideTax -> FareParameters -> FareParameters
applyRideTax rideTax fp =
  (withFareTaxes taxes fp) {isVatTaxType = Just (rideTax.regime == Tax.VAT)}
  where
    current = RideTax.fareTaxesOf fp
    bases = fareParamsBases fp
    computed = Tax.taxOnExclusive fp.currency rideTax.regime <$> rideTax.riderPayable <*> bases
    taxes =
      FB.RideTaxRates
        { rideFare = computed.rideFare,
          toll = computed.toll,
          parking = computed.parking,
          airportEntryFee = computed.airportEntryFee,
          cancellationFee = current.cancellationFee,
          paymentCharge = current.paymentCharge
        }

-- | Apply the airport entry fee as its own component, based on pickupGateId and transporter config.
applyAirportEntryFee ::
  (MonadFlow m, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, BeamFlow m r) =>
  CalculateFareParametersParams ->
  FareParameters ->
  m FareParameters
applyAirportEntryFee params fareParams = case (params.merchantOperatingCityId, params.pickupGateId) of
  (Just merchantOperatingCityId, Just gateIdText) -> do
    transporterConfig <-
      getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing
        >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
    if not (fromMaybe False transporterConfig.airportEntryFeeEnabled)
      then pure fareParams
      else do
        airportFee <- entryFeeForGateId (Id gateIdText) (Just params.farePolicy.vehicleServiceTier)
        pure $
          if airportFee > 0
            then fareParams {airportEntryFee = Just airportFee}
            else fareParams
  _ -> pure fareParams

-- | Resolve the gate's CustomerFeeItem entries into fare parameters. Each entry is
--   added to the fare sum and emitted as its own breakup line, taxed on top at the airport entry fee rate and outside discounts.
applyGateCustomerFeeItems ::
  (MonadFlow m, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, BeamFlow m r) =>
  CalculateFareParametersParams ->
  FareParameters ->
  m FareParameters
applyGateCustomerFeeItems params fareParams = case params.pickupGateId of
  Nothing -> pure fareParams
  Just gateIdText -> do
    items <- customerFeeItemsForGateId (Id gateIdText) fareParams.currency
    pure $ if KP.null items then fareParams else fareParams {customerGateFeeItems = Just items}

-- | CustomerFeeItem entries configured on a gate, skipping entries whose currency
--   does not match the fare's currency and entries with a non-positive amount.
customerFeeItemsForGateId ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DGI.GateInfo ->
  Currency ->
  m [DFParams.CustomerGateFeeItem]
customerFeeItemsForGateId gateId currency = do
  mbGate <- QGI.findById gateId
  let configuredItems = fromMaybe [] (mbGate >>= (.feeItems))
  fmap catMaybes $
    forM configuredItems $ \item ->
      if item.collectionType /= DGI.CustomerFeeItem || item.amountWithCurrency.amount <= 0
        then pure Nothing
        else
          if item.amountWithCurrency.currency /= currency
            then do
              logWarning $ "customerFeeItemsForGateId: skipping gate fee item with currency " <> show item.amountWithCurrency.currency <> " on gate " <> gateId.getId
              pure Nothing
            else
              pure $
                Just
                  DFParams.CustomerGateFeeItem
                    { itemName = fromMaybe defaultCustomerGateFeeItemName item.itemName.customer,
                      amount = item.amountWithCurrency.amount
                    }

defaultCustomerGateFeeItemName :: Text
defaultCustomerGateFeeItemName = "GATE_FEE"

-- | Entry fee for a single gate. Use when API sends gateId (e.g. SearchRequest/Booking.pickupGateId).
--   Returns 0 if gate not found, no fee configured, or the gate exempts the given
--   service tier ('Nothing' means "no tier context" and charges the configured fee).
entryFeeForGateId ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DGI.GateInfo ->
  Maybe ServiceTierType ->
  m HighPrecMoney
entryFeeForGateId gateId mbServiceTier = do
  mbGate <- QGI.findById gateId
  pure $ maybe 0 (\gate -> realToFrac $ DGI.gateEntryFeeFor gate (show <$> mbServiceTier)) mbGate

-- | Compute a configured charge (VAT, commission, or toll tax)
--
-- Steps:
-- 1. Sum up the monetary values of all components specified in config.appliesOn
-- 2. Parse the charge value (percentage like "14%" or fixed like "50")
-- 3. Apply the charge to the base amount
--
-- Example: If config = {"value":"14%","appliesOn":["RideFare","DeadKmFareComponent"]}
-- and RideFare=100, DeadKmFareComponent=20, then:
-- - baseAmount = 120
-- - result = 120 * 0.14 = 16.8
computeConfiguredCharge ::
  MonadFlow m =>
  Text -> -- Label for logging (e.g., "vatCharge", "commissionCharge")
  ComponentMap -> -- Map of all fare components to their values
  Maybe FareChargeConfig -> -- Charge configuration from fare_policy
  m HighPrecMoney
computeConfiguredCharge label componentMap = \case
  Nothing -> pure 0
  Just FareChargeConfig {..} -> do
    -- If no components specified, default to RideFare
    let baseComponents = if KP.null appliesOn then [RideFare] else appliesOn
        -- Sum up all component amounts to get the base for charge calculation
        baseAmount = sum $ fmap (componentAmount componentMap) baseComponents
    case parseCodeValue value of
      Nothing -> do
        logWarning $ "Unable to parse charge value for " <> label <> " with raw value: " <> value
        pure 0
      Just parsedValue ->
        -- Apply percentage or fixed value to base amount
        pure $ applyParsedValue baseAmount parsedValue

-- | Parse a charge value from Text (percentage like "14%" or fixed like "50")
-- Returns Nothing if the value cannot be parsed
parseCodeValue :: Text -> Maybe ParsedCodeValue
parseCodeValue rawValue =
  let trimmed = T.strip rawValue
   in if T.null trimmed
        then Nothing
        else
          if "%" `T.isSuffixOf` trimmed
            then -- Percentage value: "14%" -> ParsedPercentage 0.14

              let numeric = T.stripEnd $ T.dropEnd 1 trimmed
               in ParsedPercentage <$> parsePercentage numeric
            else -- Fixed value: "50" -> ParsedFixed 50.0
              ParsedFixed . HighPrecMoney . toRational <$> parseFixed trimmed

-- | Parse a percentage string and convert to Rational (e.g., "14" -> 0.14)
parsePercentage :: Text -> Maybe Rational
parsePercentage txt = (/ 100) . toRational <$> parseFixed txt

-- | Parse a fixed numeric string to Double (e.g., "50" -> 50.0)
-- Handles decimals and negative numbers
parseFixed :: Text -> Maybe Double
parseFixed txt =
  let numericOnly = T.filter (\c -> isDigit c || c == '.' || c == '-') txt
   in KP.readMaybe (T.unpack numericOnly)

-- | Apply a parsed charge value to a base amount
-- If percentage: multiply base by percentage (e.g., 100 * 0.14 = 14)
-- If fixed: return the fixed amount directly
applyParsedValue :: HighPrecMoney -> ParsedCodeValue -> HighPrecMoney
applyParsedValue baseAmount = \case
  ParsedPercentage pct -> HighPrecMoney (baseAmount.getHighPrecMoney * pct)
  ParsedFixed amount -> amount

-- | Build a map of all fare components from FareParameters
--
-- This map includes:
-- - Base fare components (RideFare, WaitingCharge, ServiceCharge, etc.)
-- - Detail-specific components based on fare type (Progressive, Rental, InterCity, Ambulance)
--
-- The map is used to compute charges on specific components as configured in fare_policy.
-- Example: If VAT config specifies appliesOn: ["RideFare", "DeadKmFareComponent"],
-- this map allows us to look up those components and sum them for VAT calculation.
buildComponentMap :: FareParameters -> ComponentMap
buildComponentMap FareParameters {..} =
  let maybeZero = fromMaybe 0
      -- Base map: Always includes these common fare components
      baseMap =
        Map.fromList
          [ (RideFare, baseFare),
            (WaitingCharge, maybeZero waitingCharge),
            (ServiceChargeComponent, maybeZero serviceCharge),
            (TollChargesComponent, maybeZero tollCharges),
            (CongestionChargeComponent, maybeZero congestionCharge),
            (ParkingChargeComponent, maybeZero parkingCharge),
            (PetChargeComponent, maybeZero petCharges),
            (PriorityChargeComponent, maybeZero priorityCharges),
            (NightShiftChargeComponent, maybeZero nightShiftCharge),
            (InsuranceChargeComponent, maybeZero insuranceCharge),
            (StopChargeComponent, maybeZero stopCharges),
            (LuggageChargeComponent, maybeZero luggageCharge),
            (CustomerCancellationChargeComponent, maybeZero customerCancellationDues),
            (CustomerExtraFeeComponent, maybeZero customerExtraFee),
            (AddOnChargeComponent, maybeZero addOnCharges),
            (TollVatComponent, maybeZero tollFareTax),
            (RideVatComponent, maybeZero govtCharges),
            (DriverAllowanceComponent, maybeZero driverAllowance),
            (AirportConvenienceFeeComponent, maybeZero airportConvenienceFee),
            (ReturnFeeChargeComponent, maybeZero returnFeeCharge),
            (BoothChargeComponent, maybeZero boothCharge),
            (RideExtraTimeFareComponent, maybeZero rideExtraTimeFare),
            (SchedulingChargeComponent, maybeZero schedulingCharge),
            (ParkingTaxComponent, maybeZero parkingChargeTax),
            (CardChargeOnFareComponent, fromMaybe 0 (cardCharge >>= (.onFare))),
            (CardChargeFixedComponent, fromMaybe 0 (cardCharge >>= (.fixed))),
            (ConditionalChargesComponent, sum (map (.charge) conditionalCharges)),
            (CustomerGateFeeComponent, sum (map (.amount) (fromMaybe [] customerGateFeeItems))),
            (DriverSelectedFareComponent, maybeZero driverSelectedFare),
            (NegotiatedFareDeltaComponent, maybeZero negotiatedFareDelta),
            (NegativeFareAdjustmentComponent, maybeZero negativeFareAdjustment),
            (GovtChargesComponent, maybeZero govtCharges),
            (PersonalDiscountComponent, if shouldApplyPersonalDiscount then negate (maybeZero personalDiscount) else 0),
            (BusinessDiscountComponent, if shouldApplyBusinessDiscount then negate (maybeZero businessDiscount) else 0)
          ]
   in Map.union baseMap (detailComponentMap fareParametersDetails)

detailComponentMap :: DFParams.FareParametersDetails -> ComponentMap
detailComponentMap fareParametersDetails =
  let maybeZero = fromMaybe 0
   in case fareParametersDetails of
        DFParams.ProgressiveDetails det ->
          Map.fromList
            [ (DeadKmFareComponent, det.deadKmFare),
              (ExtraKmFareComponent, maybeZero det.extraKmFare),
              (RideDurationFareComponent, maybeZero det.rideDurationFare)
            ]
        DFParams.RentalDetails det ->
          Map.fromList
            [ (TimeBasedFareComponent, det.timeBasedFare),
              (DistBasedFareComponent, det.distBasedFare),
              (DeadKmFareComponent, det.deadKmFare)
            ]
        DFParams.InterCityDetails det ->
          Map.fromList
            [ (TimeFareComponent, det.timeFare),
              (DistanceFareComponent, det.distanceFare),
              (PickupChargeComponent, det.pickupCharge),
              (ExtraDistanceFareComponent, det.extraDistanceFare),
              (ExtraTimeFareComponent, det.extraTimeFare),
              (StateEntryPermitChargesComponent, maybeZero det.stateEntryPermitCharges)
            ]
        DFParams.AmbulanceDetails det ->
          Map.fromList
            [ (AmbulanceDistBasedFareComponent, det.distBasedFare),
              (PlatformFeeComponent, maybeZero det.platformFee),
              (SgstComponent, maybeZero det.sgst),
              (CgstComponent, maybeZero det.cgst)
            ]
        DFParams.SlabDetails det ->
          Map.fromList
            [ (PlatformFeeComponent, maybeZero det.platformFee),
              (SgstComponent, maybeZero det.sgst),
              (CgstComponent, maybeZero det.cgst)
            ]

-- | Get the monetary value of a component from the component map
-- Returns 0 if the component is not found
componentAmount :: ComponentMap -> FareChargeComponent -> HighPrecMoney
componentAmount mp key = Map.findWithDefault 0 key mp

-- | Calculate commission separately (not part of fare)
--
-- This function calculates commission based on fare_policy.commission_charge_config
-- and FareParameters. Commission is NOT included in the fare sum - it's stored
-- separately in booking and ride tables for transparency.
--
-- Example: If fare_policy has commission_charge_config = {"value":"8%","appliesOn":["RideFare"]},
-- then commission will be calculated as 8% of RideFare.
--
-- Returns Nothing if commission is not configured or if amount is 0.
data CommissionBreakup = CommissionBreakup
  { rideCommission :: Maybe HighPrecMoney,
    duesCommission :: Maybe HighPrecMoney,
    totalCommission :: Maybe HighPrecMoney
  }

calculateCommission ::
  MonadFlow m =>
  FareParameters ->
  Maybe FullFarePolicy ->
  m CommissionBreakup
calculateCommission fareParams mbFarePolicy = do
  ride <- calculateRideCommission fareParams mbFarePolicy
  let dues = fareParams.customerCancellationDuesCommission
  pure
    CommissionBreakup
      { rideCommission = ride,
        duesCommission = dues,
        totalCommission = if isNothing ride && isNothing dues then Nothing else Just (fromMaybe 0 ride + fromMaybe 0 dues)
      }

calculateRideCommission ::
  MonadFlow m =>
  FareParameters ->
  Maybe FullFarePolicy ->
  m (Maybe HighPrecMoney)
calculateRideCommission fareParams mbFarePolicy = do
  case mbFarePolicy of
    Nothing -> pure Nothing
    Just farePolicy -> do
      -- Cancellation dues are never commissioned here: carried dues already carry their own
      -- commission (rider details -> search request -> fare params customerCancellationDuesCommission),
      -- and an immediate cancellation's commission comes from the consequence-matrix row booked at
      -- settlement. Including the component would charge the driver commission twice on the same dues.
      let componentMap = Map.delete CustomerCancellationChargeComponent (buildComponentMap fareParams)
      case farePolicy.commissionChargeConfig of
        Just config -> do
          commAmount <- computeConfiguredCharge "commissionCharge" componentMap (Just config)
          pure $ if commAmount > 0 then Just commAmount else Nothing
        Nothing -> pure Nothing

commissionOnComponent :: Maybe FareChargeConfig -> FareChargeComponent -> HighPrecMoney -> HighPrecMoney
commissionOnComponent mbConfig component grossAmount = case mbConfig of
  Just config
    | component /= CustomerCancellationChargeComponent,
      component `elem` (if KP.null config.appliesOn then [RideFare] else config.appliesOn),
      Just (ParsedPercentage pct) <- parseCodeValue config.value ->
      applyParsedValue grossAmount (ParsedPercentage pct)
  _ -> 0

-- | Commission on the CustomerCancellationChargeComponent alone — a past cancellation due folded
--   into this ride's fare — at its own configured rate. GROSS (ALV-inclusive); split at emission.
--   Config invariant: the component must appear in exactly one of the two commission configs —
--   keep it out of commissionChargeConfig.appliesOn or the due is commissioned twice.
--   Controlled entirely by the (Maybe) cancellationCommissionChargeConfig on the fare policy:
--   absent config => Nothing, so estimate, payout, stored values and the ledger booking stay consistent.
driverBorneAppFee :: Maybe HighPrecMoney -> Maybe Text -> HighPrecMoney
driverBorneAppFee mbCharge mbBearerText =
  case (KP.readMaybe . T.unpack) =<< mbBearerText of
    Just PAYMENT_CUSTOMER -> fromMaybe 0 mbCharge
    Just PAYMENT_DRIVER -> fromMaybe 0 mbCharge
    _ -> 0

driverEffectiveFare :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe Text -> HighPrecMoney
driverEffectiveFare mbFare mbCharge mbBearerText =
  fromMaybe 0 mbFare - driverBorneAppFee mbCharge mbBearerText

customerBorneCharge :: Maybe HighPrecMoney -> Maybe Text -> HighPrecMoney
customerBorneCharge mbCharge mbBearerText =
  case (KP.readMaybe . T.unpack) =<< mbBearerText of
    Just PAYMENT_CUSTOMER -> fromMaybe 0 mbCharge
    _ -> 0

data PaymentChargeResult = PaymentChargeResult
  { adjustedFareParams :: FareParameters,
    adjustedFare :: HighPrecMoney,
    paymentCharge :: Maybe HighPrecMoney,
    paymentChargeBearer :: Maybe Text
  }

-- | The payment charge on what the customer pays (fare without any charge, less
--   the offer discount), from the config the BAP sent for the chosen payment method.
paymentChargeQuoteFor :: Currency -> Maybe PaymentChargeDetails -> Maybe HighPrecMoney -> HighPrecMoney -> Maybe PaymentChargeQuote
paymentChargeQuoteFor fareCurrency mbConfig mbDiscount fareWithoutCharge =
  mbConfig >>= \config -> quotePaymentCharge fareCurrency config (max 0 (fareWithoutCharge - fromMaybe 0 mbDiscount))

-- | Set the payment charge on the fare params: the PAYMENT_CHARGE fare component
--   when the customer bears it, cleared otherwise (the bearer then drives the
--   driver/platform deduction at ride end).
applyPaymentChargeDetails :: Maybe PaymentChargeDetails -> Maybe HighPrecMoney -> FareParameters -> FareParameters
applyPaymentChargeDetails mbConfig mbDiscount fareParams =
  withPaymentChargeQuote (paymentChargeQuoteFor fareParams.currency mbConfig mbDiscount (fareSum cleared Nothing)) cleared
  where
    cleared = withPaymentChargeQuote Nothing fareParams

paymentChargeResult :: Maybe PaymentChargeDetails -> Maybe HighPrecMoney -> HighPrecMoney -> FareParameters -> PaymentChargeResult
paymentChargeResult mbConfig mbDiscount currentFare fareParams =
  PaymentChargeResult
    { adjustedFareParams = withPaymentChargeQuote mbQuote fareParams,
      adjustedFare = fareOnly + customerPaymentChargeGross mbQuote,
      paymentCharge = paymentChargeGross <$> mbQuote,
      paymentChargeBearer = show . (.bearer) <$> mbQuote
    }
  where
    fareOnly = max 0 (currentFare - fromMaybe 0 fareParams.paymentProcessingFee - fromMaybe 0 fareParams.paymentProcessingFeeTax)
    mbQuote = paymentChargeQuoteFor fareParams.currency mbConfig mbDiscount fareOnly

withPaymentChargeQuote :: Maybe PaymentChargeQuote -> FareParameters -> FareParameters
withPaymentChargeQuote mbQuote fareParams = case mbQuote of
  Just quote
    | quote.bearer == PAYMENT_CUSTOMER ->
      fareParams
        { paymentProcessingFee = Just quote.net,
          paymentProcessingFeeTax = Just quote.tax,
          paymentChargeTaxCgst = quote.cgst,
          paymentChargeTaxSgst = quote.sgst,
          paymentChargeTaxIgst = quote.igst
        }
  _ ->
    fareParams
      { paymentProcessingFee = Nothing,
        paymentProcessingFeeTax = Nothing,
        paymentChargeTaxCgst = Nothing,
        paymentChargeTaxSgst = Nothing,
        paymentChargeTaxIgst = Nothing
      }

clampDiscountToDiscountable :: FareParameters -> Maybe HighPrecMoney -> Maybe HighPrecMoney
clampDiscountToDiscountable fareParams mbRaw = do
  raw <- mbRaw
  let edcCollects = FB.RideTaxRates {rideFare = False, toll = False, parking = True, airportEntryFee = True, cancellationFee = False, paymentCharge = False}
      edcLine l collects isCharge titles = collects && (isCharge l.title || l.title `elem` titles)
      edcCollected l = SL.edcCollectsParking fareParams.fareSettlementType && or (edcLine l <$> edcCollects <*> FB.isChargeTitle <*> FB.taxTitles)
      breakupLines = filter (not . edcCollected) (mkFareParamsBreakups True (\amount -> amount) FB.BreakupLine fareParams)
      clamped = max 0 (min raw (FB.discountableAmount (fareSum fareParams Nothing) breakupLines))
  if clamped > 0 then Just clamped else Nothing
