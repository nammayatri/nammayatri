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
    pureFareSum,
    perRideKmFareParamsSum,
    getPerMinuteRate,
    CalculateFareParametersParams (..),
    isNightShift,
    isNightAllowanceApplicable,
    timeZoneIST,
    UTCTime (UTCTime, utctDay),
    calculateCancellationCharges,
    calculateNoShowCharges,
    computeRideDiscount,
    computeTotalGstRate,
    countFullFareOfParamsDetails,
    calculateFareParameters,
    calculateCommission,
    mkFareParamsBreakups,
    mkFareParamsDisplayBreakups,
    mkProjectFareParamsTagBreakupItems,
    buildComponentMap,
    componentAmount,
    discountApplicableComponents,
    projectFareParamsBreakup,
    clampDiscountToDiscountable,
    ComponentMap,
    entryFeeForGateId,
  )
where

import "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant hiding (NightShiftChargeAPIEntity (..), VehicleVariant (..), WaitingChargeAPIEntity (..))
import qualified BecknV2.OnDemand.Enums as Enums
import Data.Char (isDigit)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Domain.SharedLogic.RideDiscount as RD
import Domain.Types.CancellationFarePolicy as DTCFP
import Domain.Types.Common
import qualified Domain.Types.ConditionalCharges as DAC
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as DFP
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.TransporterConfig as DTC
import EulerHS.Prelude hiding (elem, id, map, sum)
import GHC.Float (int2Double)
import Kernel.Prelude as KP
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Price as Price
import Kernel.Utils.Common hiding (isTimeWithinBounds, mkPrice)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Types.GateInfo as DGI
import qualified Storage.Cac.TransporterConfig as SCTC

-- | Full quotation.breakup for a 'FareParameters': display tags followed by
--   the canonical eight-tag summary. Callers who want finer control can
--   use 'mkFareParamsDisplayBreakups' and 'mkProjectFareParamsTagBreakupItems'
--   separately.
mkFareParamsBreakups :: (HighPrecMoney -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkFareParamsBreakups mkPrice mkBreakupItem fareParams =
  mkFareParamsDisplayBreakups mkPrice mkBreakupItem fareParams
    <> mkProjectFareParamsTagBreakupItems mkPrice mkBreakupItem fareParams

-- | Canonical ten-tag summary only. Exposed so callers (e.g. rate-card
--   tag builders) can emit the summary with a custom 'mkBreakupItem'
--   constructor — the rate card uses 'mkRateCardBreakupItem' here so the
--   summary tag titles are preserved verbatim (no @_FARE_PARAM@ suffix),
--   unlike the display tags which go through
--   'mkRateCardFareParamsBreakupItem'. Returns empty when the
--   'projectFareParamsBreakup' projection is absent (pure GST mode).
mkProjectFareParamsTagBreakupItems ::
  (HighPrecMoney -> breakupItemPrice) ->
  (Text -> breakupItemPrice -> breakupItem) ->
  FareParameters ->
  [breakupItem]
mkProjectFareParamsTagBreakupItems mkPrice mkBreakupItem fareParams =
  case projectFareParamsBreakup fareParams of
    Nothing -> []
    Just b ->
      [ mkBreakupItem (show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE) (mkPrice b.discountApplicableRideFareTaxExclusive),
        mkBreakupItem (show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX) (mkPrice b.discountApplicableRideFareTax),
        mkBreakupItem (show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE) (mkPrice b.nonDiscountApplicableRideFareTaxExclusive),
        mkBreakupItem (show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX) (mkPrice b.nonDiscountApplicableRideFareTax),
        mkBreakupItem (show Enums.TOLL_FARE_TAX_EXCLUSIVE) (mkPrice b.tollFareTaxExclusive),
        mkBreakupItem (show Enums.TOLL_FARE_TAX) (mkPrice b.tollFareTax),
        mkBreakupItem (show Enums.CANCELLATION_FEE_TAX_EXCLUSIVE) (mkPrice b.cancellationFeeTaxExclusive),
        mkBreakupItem (show Enums.CANCELLATION_TAX) (mkPrice b.cancellationTax),
        mkBreakupItem (show Enums.PARKING_CHARGE_TAX_EXCLUSIVE) (mkPrice b.parkingChargeTaxExclusive),
        mkBreakupItem (show Enums.PARKING_CHARGE_TAX) (mkPrice b.parkingChargeTax)
      ]

-- | Display-tag portion of the quotation.breakup (BASE_FARE,
--   DISTANCE_FARE, PARKING_CHARGE, TOLL_CHARGES, …). Does NOT include
--   the canonical eight-tag summary — 'mkFareParamsBreakups' concats both.
mkFareParamsDisplayBreakups :: (HighPrecMoney -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkFareParamsDisplayBreakups mkPrice mkBreakupItem fareParams = do
  -- let dayPartRate = fromMaybe 1.0 fareParams.nightShiftRateIfApplies -- Temp fix :: have to fix properly
  let baseFareFinal = HighPrecMoney $ fareParams.baseFare.getHighPrecMoney -- Temp fix :: have to fix properly
      baseFareCaption = show Enums.BASE_FARE
      baseFareItem = mkBreakupItem baseFareCaption (mkPrice baseFareFinal)

      serviceChargeCaption = show Enums.SERVICE_CHARGE
      mbServiceChargeItem = fmap (mkBreakupItem serviceChargeCaption) (mkPrice <$> fareParams.serviceCharge)

      congestionChargeCaption = show Enums.CONGESTION_CHARGE
      mbCongestionChargeItem = fmap (mkBreakupItem congestionChargeCaption) (mkPrice <$> fareParams.congestionCharge)

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

      -- ONDC v2.1.0: duplicate with new title for spec compliance
      nightChargesCaption = show Enums.NIGHT_CHARGES
      mbNightChargesItem = fmap (mkBreakupItem nightChargesCaption) (mkPrice <$> fareParams.nightShiftCharge)

      parkingChargeCaption = show Enums.PARKING_CHARGE
      mbParkingChargeItem = mkBreakupItem parkingChargeCaption . mkPrice <$> fareParams.parkingCharge

      -- ONDC v2.1.0: duplicate with new title for spec compliance
      parkingChargesCaption = show Enums.PARKING_CHARGES
      mbParkingChargesItem = mkBreakupItem parkingChargesCaption . mkPrice <$> fareParams.parkingCharge

      waitingChargesCaption = show Enums.WAITING_OR_PICKUP_CHARGES
      mbWaitingChargesItem = mkBreakupItem waitingChargesCaption . mkPrice <$> fareParams.waitingCharge

      -- ONDC v2.1.0: duplicate with new title for spec compliance
      waitingChargesCaption2 = show Enums.WAITING_CHARGES
      mbWaitingChargesItem2 = mkBreakupItem waitingChargesCaption2 . mkPrice <$> fareParams.waitingCharge

      mbFixedGovtRateCaption = show Enums.FIXED_GOVERNMENT_RATE
      mbFixedGovtRateItem = mkBreakupItem mbFixedGovtRateCaption . mkPrice <$> fareParams.govtCharges

      -- Backward compat: emit RIDE_VAT for VAT-type rides (isVatTaxType=True)
      -- govtCharges holds the merged VAT value (set by FareCalculatorV2)
      mbRideVatCaption = show Enums.RIDE_VAT
      mbRideVatItem =
        if fromMaybe False fareParams.isVatTaxType
          then mkBreakupItem mbRideVatCaption . mkPrice <$> fareParams.govtCharges
          else Nothing

      customerCancellationDuesCaption = show Enums.CANCELLATION_CHARGES
      mbCustomerCancellationDues = mkBreakupItem customerCancellationDuesCaption . mkPrice <$> fareParams.customerCancellationDues

      tollChargesCaption = show Enums.TOLL_CHARGES
      mbTollChargesItem = mkBreakupItem tollChargesCaption . mkPrice <$> fareParams.tollCharges

      petChargesCaption = show Enums.PET_CHARGES
      mbPetChargesItem = mkBreakupItem petChargesCaption . mkPrice <$> fareParams.petCharges

      driverAllowanceCaption = show Enums.DRIVER_ALLOWANCE
      mbDriverAllowanceItem = mkBreakupItem driverAllowanceCaption . mkPrice <$> fareParams.driverAllowance

      airportConvenienceFeeCaption = show Enums.AIRPORT_CONVENIENCE_FEE
      mbAirportConvenienceFeeItem = mkBreakupItem airportConvenienceFeeCaption . mkPrice <$> fareParams.airportConvenienceFee

      mkBusinessDiscountCaption = show Enums.BUSINESS_DISCOUNT
      mbBusinessDiscountItem = mkBreakupItem mkBusinessDiscountCaption . mkPrice <$> fareParams.businessDiscount

      mkPersonalDiscountCaption = show Enums.PERSONAL_DISCOUNT
      mbPersonalDiscountItem = mkBreakupItem mkPersonalDiscountCaption . mkPrice <$> fareParams.personalDiscount

      priorityChargesCaption = show Enums.PRIORITY_CHARGES
      mbPriorityChargesItem = mkBreakupItem priorityChargesCaption . mkPrice <$> fareParams.priorityCharges

      insuranceChargeCaption = show Enums.INSURANCE_CHARGES
      mbInsuranceChargeItem = mkBreakupItem insuranceChargeCaption . mkPrice <$> fareParams.insuranceCharge

      cardChargesFareCaption = show Enums.CARD_CHARGES_ON_FARE
      mbCardChargesFareItem = fareParams.cardCharge >>= \cardCharge -> mkBreakupItem cardChargesFareCaption . mkPrice <$> cardCharge.onFare

      cardChargesFixedCaption = show Enums.CARD_CHARGES_FIXED
      mbCardChargesFixedItem = fareParams.cardCharge >>= \cardCharge -> mkBreakupItem cardChargesFixedCaption . mkPrice <$> cardCharge.fixed

      rideStopChargeCaption = show Enums.RIDE_STOP_CHARGES
      mbRideStopChargeItem = mkBreakupItem rideStopChargeCaption . mkPrice <$> fareParams.stopCharges

      luggageChargeCaption = show Enums.LUGGAGE_CHARGE
      mbLuggageChargeItem = mkBreakupItem luggageChargeCaption . mkPrice <$> fareParams.luggageCharge

      returnFeeChargeCaption = show Enums.RETURN_FEE
      mbReturnFeeChargeItem = mkBreakupItem returnFeeChargeCaption . mkPrice <$> fareParams.returnFeeCharge

      boothChargeCaption = show Enums.BOOTH_CHARGE
      mbBoothChargeItem = mkBreakupItem boothChargeCaption . mkPrice <$> fareParams.boothCharge

      tollVatCaption = show Enums.TOLL_VAT
      mbTollVatItem = mkBreakupItem tollVatCaption . mkPrice <$> fareParams.tollFareTax

      detailsBreakups = processFareParamsDetails fareParams.fareParametersDetails
      additionalChargesBreakup = map (\addCharges -> mkBreakupItem (show $ castAdditionalChargeCategoriesToEnum addCharges.chargeCategory) $ mkPrice addCharges.charge) fareParams.conditionalCharges
  catMaybes
    [ Just baseFareItem,
      mbCongestionChargeItem,
      mbNightShiftChargeItem,
      mbNightChargesItem,
      mbParkingChargeItem,
      mbParkingChargesItem,
      mbWaitingChargesItem,
      mbWaitingChargesItem2,
      mbBusinessDiscountItem,
      mbPersonalDiscountItem,
      mbFixedGovtRateItem,
      mbRideVatItem,
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
      mbRideStopChargeItem,
      mbLuggageChargeItem,
      mbReturnFeeChargeItem,
      mbBoothChargeItem,
      mbTollVatItem
    ]
    <> detailsBreakups
    <> additionalChargesBreakup
  where
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
          sgstCaption = show Enums.SGST
          mbSgstItem = mkBreakupItem sgstCaption . mkPrice <$> det.sgst
          cgstCaption = show Enums.CGST
          mbCgstItem = mkBreakupItem cgstCaption . mkPrice <$> det.cgst
      catMaybes [mbPlatformFeeItem, mbSgstItem, mbCgstItem]

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
          sgstCaption = show Enums.SGST
          mbSgstItem = mkBreakupItem sgstCaption . mkPrice <$> det.sgst
          cgstCaption = show Enums.CGST
          mbCgstItem = mkBreakupItem cgstCaption . mkPrice <$> det.cgst
          distBasedCaption = show Enums.DIST_BASED_FARE
          mbDistBasedFare = mkBreakupItem distBasedCaption (mkPrice det.distBasedFare)
      catMaybes [mbPlatformFeeItem, mbSgstItem, mbCgstItem, Just mbDistBasedFare]

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

-- TODO: make some tests for it

fareSum :: FareParameters -> Maybe [DAC.ConditionalChargesCategories] -> HighPrecMoney
fareSum fareParams conditionalChargeCategories = do
  pureFareSum fareParams conditionalChargeCategories
    + fromMaybe 0.0 fareParams.driverSelectedFare
    + fromMaybe 0.0 fareParams.customerExtraFee
    - (if fareParams.shouldApplyBusinessDiscount then fromMaybe 0.0 fareParams.businessDiscount else 0.0)
    - (if fareParams.shouldApplyPersonalDiscount then fromMaybe 0.0 fareParams.personalDiscount else 0.0)

-- Pure fare without customerExtraFee and driverSelectedFare

-- | Calculate the pure fare sum (final total fare amount)
--
-- Includes all fare components:
-- - Base fare components (baseFare, serviceCharge, waitingCharge, etc.)
-- - Additional charges (petCharges, stopCharges, priorityCharges, etc.)
-- - Policy-specific details (progressive/rental/intercity components)
-- - VAT charges (tollVat) from calculateFareParameters (rideVat is merged into govtCharges)
-- - Payment processing fee
-- - Conditional charges (filtered by category)
--
-- Note: Commission is NOT part of FareParameters and is NOT included in the final sum.
-- Commission is calculated separately and stored in Booking/Ride tables.
-- It is calculated and stored for breakdown/transparency purposes only,
-- as per PRD requirements.
pureFareSum :: FareParameters -> Maybe [DAC.ConditionalChargesCategories] -> HighPrecMoney
pureFareSum fareParams conditionalChargeCategories = do
  let (partOfNightShiftCharge, notPartOfNightShiftCharge, platformFee) = countFullFareOfParamsDetails fareParams.fareParametersDetails
  fareParams.baseFare
    + fromMaybe 0.0 fareParams.serviceCharge
    + fromMaybe 0.0 fareParams.waitingCharge
    + fromMaybe 0.0 fareParams.govtCharges
    + fromMaybe 0.0 fareParams.nightShiftCharge
    + fromMaybe 0.0 fareParams.rideExtraTimeFare
    + fromMaybe 0.0 fareParams.congestionCharge
    + fromMaybe 0.0 fareParams.petCharges
    + fromMaybe 0.0 fareParams.driverAllowance
    + fromMaybe 0.0 fareParams.airportConvenienceFee
    + fromMaybe 0.0 fareParams.stopCharges
    + fromMaybe 0.0 fareParams.priorityCharges
    + partOfNightShiftCharge
    + notPartOfNightShiftCharge
    + platformFee
    + (fromMaybe 0.0 fareParams.customerCancellationDues + fromMaybe 0.0 fareParams.tollCharges + fromMaybe 0.0 fareParams.parkingCharge)
    + fromMaybe 0.0 fareParams.insuranceCharge
    + fromMaybe 0.0 fareParams.luggageCharge
    + fromMaybe 0.0 fareParams.returnFeeCharge
    + fromMaybe 0.0 fareParams.boothCharge
    + fromMaybe 0.0 (fareParams.cardCharge >>= (.onFare))
    + fromMaybe 0.0 (fareParams.cardCharge >>= (.fixed))
    + fromMaybe 0.0 fareParams.paymentProcessingFee
    + fromMaybe 0.0 fareParams.tollFareTax
    + fromMaybe 0.0 fareParams.parkingChargeTax
    -- Commission is intentionally excluded - stored for breakdown only
    + (sum $ map (.charge) (filter (\addCharges -> maybe True (KP.elem addCharges.chargeCategory) conditionalChargeCategories) fareParams.conditionalCharges))

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

data CalculateFareParametersParams = CalculateFareParametersParams
  { farePolicy :: FullFarePolicy,
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
    nightShiftCharge :: Maybe HighPrecMoney,
    customerCancellationDues :: Maybe HighPrecMoney,
    estimatedRideDuration :: Maybe Seconds,
    estimatedCongestionCharge :: Maybe HighPrecMoney,
    nightShiftOverlapChecking :: Bool,
    estimatedDistance :: Maybe Meters,
    timeDiffFromUtc :: Maybe Seconds,
    tollCharges :: Maybe HighPrecMoney,
    noOfStops :: Int,
    currency :: Currency,
    distanceUnit :: DistanceUnit,
    petCharges :: Maybe HighPrecMoney,
    shouldApplyBusinessDiscount :: Bool,
    shouldApplyPersonalDiscount :: Bool,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    mbAdditonalChargeCategories :: Maybe [DAC.ConditionalChargesCategories],
    numberOfLuggages :: Maybe Int,
    govtChargesRate :: Maybe DTC.GstBreakup, -- from TaxConfig.rideGst; summed inside calculateFareParameters
    pickupGateId :: Maybe Text -- Optional airport pickup gate id; used by V2 to apply airport entry fee
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
      (debugLogs, baseFare, nightShiftCharge, waitingChargeInfo, fareParametersDetails) = processFarePolicyDetails fp.farePolicyDetails
      (partOfNightShiftCharge, notPartOfNightShiftCharge, _) = countFullFareOfParamsDetails fareParametersDetails
      fullRideCost {-without govtCharges, serviceCharge, platformFee, waitingCharge, notPartOfNightShiftCharge, nightShift, insuranceCharge, cardChargeOnFare and fixedCardCharge-} =
        baseFare
          + partOfNightShiftCharge
  let resultFullNightShiftCharge = if isNightShiftChargeIncluded then countNightShiftCharge fullRideCost <$> nightShiftCharge else Nothing
  logDebug $ "NightShiftChanges : " <> "resultFullNightShiftCharge: " <> show resultFullNightShiftCharge
  let resultNightShiftCharge = if nightShiftBuffer then calNightShiftCharge resultFullNightShiftCharge rideTimeWithBuffer fp.nightShiftBounds rideDur else resultFullNightShiftCharge
  logDebug $ "NightShiftChanges : " <> "resultNightShiftCharge: " <> show resultNightShiftCharge
  let resultWaitingCharge = countWaitingCharge =<< waitingChargeInfo
      congestionChargeByMultiplier =
        fp.congestionChargeMultiplier <&> \case
          DFP.BaseFareAndExtraDistanceFare congestionCharge -> HighPrecMoney (fullRideCost.getHighPrecMoney * toRational congestionCharge) - fullRideCost
          DFP.ExtraDistanceFare congestionCharge -> HighPrecMoney (partOfNightShiftCharge.getHighPrecMoney * toRational congestionCharge) - partOfNightShiftCharge
      congestionChargeByPerMin =
        fp.congestionChargePerMin >>= \congestionChargePerMin ->
          let duration = params.estimatedRideDuration <|> params.actualRideDuration
           in duration >>= \dur -> Just $ HighPrecMoney (realToFrac (fromIntegral dur / 60 * congestionChargePerMin))
      congestionChargeResult = congestionChargeByPerMin <|> congestionChargeByMultiplier
      congestionChargeResultWithAddition = fromMaybe 0.0 congestionChargeResult + fp.additionalCongestionCharge
      finalCongestionCharge = fromMaybe 0.0 (params.estimatedCongestionCharge <|> Just congestionChargeResultWithAddition)
      insuranceChargeResult = countInsuranceChargeForDistance fp.distanceUnit params.actualDistance fp.perDistanceUnitInsuranceCharge
      -- petCharges = if params.isPetRide then fp.petCharges else Nothing
      luggageCharge = case (fp.perLuggageCharge, params.numberOfLuggages) of
        (Just perLuggageCharge, Just numberOfLuggages) -> Just $ perLuggageCharge * fromIntegral numberOfLuggages
        _ -> Nothing
      returnFeeCharge = case fp.returnFee of
        Just (DFP.ReturnFeeFixed fee) -> Just fee
        Just (DFP.ReturnFeePercentage p) -> Just $ partOfNightShiftCharge * fromRational (toRational p / 100)
        _ -> Nothing
      boothCharge = case fp.boothCharges of
        Just (DFP.BoothChargeFixed fee) -> Just fee
        Just (DFP.BoothChargePercentage p) -> Just $ partOfNightShiftCharge * fromRational (toRational p / 100)
        _ -> Nothing
      fullRideCostN {-without govtCharges, platformFee, cardChargeOnFare and fixedCharge-} =
        fullRideCost
          + fromMaybe 0.0 resultNightShiftCharge
          + fromMaybe 0.0 resultWaitingCharge
          + finalCongestionCharge ----------Needs to be changed to congestionChargeResult
          + fromMaybe 0.0 params.petCharges
          + fromMaybe 0.0 fp.driverAllowance
          + fromMaybe 0.0 fp.airportConvenienceFee
          + fromMaybe 0.0 fp.serviceCharge
          + fromMaybe 0.0 fp.priorityCharges
          + fromMaybe 0.0 insuranceChargeResult
          + notPartOfNightShiftCharge
      govtChargesRate' = params.govtChargesRate >>= computeTotalGstRate
      govtCharges =
        HighPrecMoney . (fullRideCostN.getHighPrecMoney *) . toRational <$> govtChargesRate'
      stopCharges =
        HighPrecMoney . ((toRational params.noOfStops) *) . toRational <$> fp.perStopCharge
      extraTimeFareInfo = calculateExtraTimeFare params.estimatedRideDuration fp.rideExtraTimeChargeGracePeriod fp.perMinuteRideExtraTimeCharge params.actualRideDuration
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
            customerExtraFee = params.customerExtraFee,
            shouldApplyBusinessDiscount = params.shouldApplyBusinessDiscount,
            shouldApplyPersonalDiscount = params.shouldApplyPersonalDiscount,
            serviceCharge = fp.serviceCharge,
            parkingCharge = fp.parkingCharge,
            baseFare = baseFare,
            petCharges = params.petCharges,
            driverAllowance = fp.driverAllowance,
            airportConvenienceFee = fp.airportConvenienceFee,
            priorityCharges = fp.priorityCharges,
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
            customerCancellationDues = params.customerCancellationDues,
            tollCharges = addMaybes fp.tollCharges (if isTollApplicableForTrip fp.vehicleServiceTier fp.tripCategory then params.tollCharges else Nothing),
            govtCharges = govtCharges,
            insuranceCharge = insuranceChargeResult,
            luggageCharge = luggageCharge,
            returnFeeCharge = returnFeeCharge,
            boothCharge = boothCharge,
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
            platformFeeChargesBy = fp.platformFeeChargesBy,
            merchantId = Just params.farePolicy.merchantId,
            merchantOperatingCityId = params.merchantOperatingCityId,
            conditionalCharges = filter (\addCharges -> maybe True (\chargesCategories -> addCharges.chargeCategory `elem` chargesCategories) params.mbAdditonalChargeCategories) params.farePolicy.conditionalCharges,
            driverCancellationPenaltyAmount = fp.driverCancellationPenaltyAmount,
            businessDiscount = businessDiscount,
            personalDiscount = personalDiscount,
            paymentProcessingFee = Nothing,
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
            parkingChargeTax = Nothing
          }
  KP.forM_ debugLogs $ logTagInfo ("FareCalculator:FarePolicyId:" <> show fp.id.getId)
  logTagInfo "FareCalculator" $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
  where
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
              currency = currency,
              distBasedFare
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
          reservedTime = case params.returnTime of
            Just rt -> max estimatedDuration (round $ diffUTCTime rt params.rideTime)
            _ -> estimatedDuration
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
            { timeFare = fareByTime_,
              distanceFare = fareByDist_,
              pickupCharge = deadKmFare,
              extraDistanceFare,
              extraTimeFare,
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
            { timeBasedFare = fareByTime,
              distBasedFare = fareByDist,
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
          (extraKmFare, baseFareDepreciation) = maybe (HighPrecMoney 0.0, HighPrecMoney 0.0) (processFPProgressiveDetailsPerExtraKmFare perExtraKmRateSections) mbExtraDistance
          (mbRideDurationFare, debugLogs) = maybe (Nothing, []) (processFPProgressiveDetailsPerRideDurationMinFare perMinRateSections) mbEstimatedRideDurationInMins
      ( debugLogs,
        baseFare + baseFareDepreciation,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.ProgressiveDetails $
          DFParams.FParamsProgressiveDetails
            { extraKmFare = if extraKmFare == 0.0 then Nothing else Just extraKmFare,
              rideDurationFare = mbRideDurationFare,
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
        DFParams.SlabDetails $ maybe (FParamsSlabDetails Nothing Nothing Nothing det.currency) countPlatformFeeMath platformFeeInfo
      (DFParams.AmbulanceDetails det) ->
        DFParams.AmbulanceDetails $ maybe (FParamsAmbulanceDetails Nothing Nothing Nothing det.distBasedFare det.currency) (countPlatformFeeAmbulance det) platformFeeInfo
      where
        countPlatformFeeMath platformFeeInfo' = do
          let (platformFee, cgst, sgst) = getPlatformFee platformFeeInfo'
          FParamsSlabDetails {..}
        countPlatformFeeAmbulance det platformFeeInfo' = do
          let (platformFee, cgst, sgst) = getPlatformFee platformFeeInfo'
          FParamsAmbulanceDetails {distBasedFare = det.distBasedFare, currency = det.currency, ..}
        getPlatformFee platformFeeInfo' = do
          let baseFee = case platformFeeInfo'.platformFeeCharge of
                ProgressivePlatformFee charge -> fullCompleteRideCost * charge
                ConstantPlatformFee charge -> charge
              cgst = Just . HighPrecMoney . toRational $ platformFeeInfo'.cgst * realToFrac baseFee
              sgst = Just . HighPrecMoney . toRational $ platformFeeInfo'.sgst * realToFrac baseFee
          (Just baseFee, cgst, sgst)

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
  DFParams.SlabDetails det -> (0.0, 0.0, fromMaybe 0.0 det.platformFee + fromMaybe 0.0 det.sgst + fromMaybe 0.0 det.cgst)
  DFParams.RentalDetails det -> (0.0, det.distBasedFare + det.timeBasedFare + det.deadKmFare, 0.0)
  DFParams.InterCityDetails det -> (0.0, det.pickupCharge + det.distanceFare + det.timeFare + det.extraDistanceFare + det.extraTimeFare + fromMaybe 0.0 det.stateEntryPermitCharges, 0.0)
  DFParams.AmbulanceDetails det -> (det.distBasedFare, 0.0, fromMaybe 0.0 det.platformFee + fromMaybe 0.0 det.sgst + fromMaybe 0.0 det.cgst)

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

-- Main function to calculate total hours with 14-hour daily cap
calculateAllowanceMins :: Seconds -> Int -> UTCTime -> UTCTime -> Int
calculateAllowanceMins timeDiffFromUtc perDayMaxAllowanceInMins startTime endTime =
  sum $ map (min perDayMaxAllowanceInMins . hoursInDay startTime endTime startIST endIST) [startDay .. endDay]
  where
    startIST = utcToIst (secondsToMinutes timeDiffFromUtc) startTime
    endIST = utcToIst (secondsToMinutes timeDiffFromUtc) endTime
    startDay = localDay startIST
    endDay = localDay endIST

calculateCancellationCharges :: DTCFP.CancellationFarePolicy -> Maybe Meters -> Maybe Meters -> Int -> HighPrecMoney -> HighPrecMoney
calculateCancellationCharges cancellationAndNoShowConfigs initialDistanceToPickup currDistanceToPickup timeSpentByDriver estimatedFare = do
  case (currDistanceToPickup, initialDistanceToPickup) of
    (Just currDist, Just initDist) -> do
      let distanceTravelledByDriver = initDist - currDist
      if distanceTravelledByDriver.getMeters > 0
        then
          let distanceCharges = cancellationAndNoShowConfigs.perMetreCancellationCharge * toHighPrecMoney distanceTravelledByDriver.getMeters
              timeCharges = (toHighPrecMoney timeSpentByDriver / 60) * cancellationAndNoShowConfigs.perMinuteCancellationCharge
              percentageOfRideFare = toHighPrecMoney cancellationAndNoShowConfigs.percentageOfRideFareToBeCharged * estimatedFare
              timeAndDistanceCharges = distanceCharges + timeCharges
              minCharge = cancellationAndNoShowConfigs.minCancellationCharge
              maxCharge = cancellationAndNoShowConfigs.maxCancellationCharge
              cancellationFee = max minCharge (min timeAndDistanceCharges (min percentageOfRideFare maxCharge))
           in cancellationFee
        else cancellationAndNoShowConfigs.minCancellationCharge
    _ -> cancellationAndNoShowConfigs.minCancellationCharge

calculateNoShowCharges :: Maybe UTCTime -> Maybe DTCFP.CancellationFarePolicy -> UTCTime -> Maybe HighPrecMoney
calculateNoShowCharges mbDriverArrivalTime mbCancellationAndNoShowConfigs now = do
  case (mbDriverArrivalTime, mbCancellationAndNoShowConfigs) of
    (Just arrivalTime, Just cancellationAndNoShowConfigs) -> do
      let timeDiff = roundToIntegral $ diffUTCTime now arrivalTime
      if timeDiff > cancellationAndNoShowConfigs.maxWaitingTimeAtPickupSeconds
        then
          let maxWaitingTimeAtPickupMinutes = fromIntegral (cancellationAndNoShowConfigs.maxWaitingTimeAtPickupSeconds.getSeconds `div` 60)
              cancellationFee = cancellationAndNoShowConfigs.maxCancellationCharge + (maxWaitingTimeAtPickupMinutes * cancellationAndNoShowConfigs.perMinuteCancellationCharge)
           in Just cancellationFee
        else Nothing
    _ -> Nothing

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

-- | Calculate fare parameters with configurable charges (VAT, toll tax)
--
-- This function:
-- 1. Calls the base v1 fare calculation to get standard fare components
-- 2. Checks if enableFareCalculatorV2 is enabled in TransporterConfig
-- 3. If enabled, applies configurable charges (VAT, toll tax) based on fare_policy configuration
-- 4. If disabled, returns the base fare parameters (fallback to v1 behavior)
-- 5. Returns FareParameters with new breakdown fields (rideVat, tollVat)
--
-- Note: Commission is NOT stored in FareParameters. It's calculated separately using
-- calculateCommission and stored in Booking/Ride tables.
--
-- The new charges are stored separately in fare_parameters table for transparency,
-- and are included in pureFareSum for final fare calculation.
--
-- Example: If fare_policy has vat_charge_config = {"value":"14%","appliesOn":["RideFare","DeadKmFareComponent"]},
-- then VAT will be calculated as 14% of (RideFare + DeadKmFareComponent)
calculateFareParameters ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, BeamFlow m r) =>
  CalculateFareParametersParams ->
  m FareParameters
calculateFareParameters params = do
  -- First, calculate base fare using v1 calculator
  baseFareParams <- calculateFareParametersHandler params
  -- Check if V2 features are enabled via TransporterConfig
  isV2Enabled <- case params.merchantOperatingCityId of
    Just merchantOpCityId -> do
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing
      let v2Enabled = maybe False (fromMaybe False . (.enableFareCalculatorV2)) transporterConfig
      logDebug $ "FareCalculator: TransporterConfig for merchantOpCityId " <> merchantOpCityId.getId <> " - enableFareCalculatorV2: " <> show (transporterConfig >>= (.enableFareCalculatorV2)) <> ", V2 enabled: " <> show v2Enabled
      pure v2Enabled
    Nothing -> do
      logDebug "FareCalculator: No merchantOperatingCityId provided, using v1 behavior"
      pure False -- If no merchantOperatingCityId, default to v1 behavior
      -- Apply configurable charges only if V2 is enabled
  fareWithV2 <-
    if isV2Enabled
      then applyConfiguredCharges params.farePolicy baseFareParams
      else do
        logDebug "FareCalculator: V2 disabled or not enabled, using v1 behavior (no configurable charges applied)"
        pure baseFareParams
  -- Apply airport entry fee (if any) to parkingCharge in FareParameters
  applyAirportEntryFee params fareWithV2

-- | Apply configurable charges (VAT, commission, toll tax) to fare parameters
--
-- Builds a component map from fare parameters and computes charges based on
-- fare_policy configuration. Charges are only calculated if explicitly configured.
--
-- Note: Commission is calculated and stored for breakdown/transparency but is NOT
-- added to the final fare sum (as per PRD requirements).
applyConfiguredCharges :: MonadFlow m => FullFarePolicy -> FareParameters -> m FareParameters
applyConfiguredCharges farePolicy fareParams = do
  let componentMap = buildComponentMap fareParams
      -- Ride VAT is split: VAT on components that are BOTH discount-applicable
      -- AND in vatChargeConfig.appliesOn goes to 'discountApplicableRideFareTax';
      -- VAT on the remainder of appliesOn (those not in the discount list) goes
      -- to 'nonDiscountApplicableRideFareTax'. Toll / cancellation components
      -- are excluded from the non-discount ride VAT — they have their own
      -- buckets and their own VAT configs.
      taxableSet = maybe [] (.appliesOn) farePolicy.vatChargeConfig
      parkingTaxable = ParkingChargeComponent `elem` taxableSet
      discTaxableBase = sum [componentAmount componentMap c | c <- taxableSet, c `elem` discountApplicableComponents]
      -- Parking has its own bucket (parkingChargeTaxExclusive/parkingChargeTax)
      -- so exclude ParkingChargeComponent here to avoid double-counting its
      -- VAT into the non-discount ride tax.
      nonDiscTaxableBase =
        sum
          [ componentAmount componentMap c
            | c <- taxableSet,
              c `KP.notElem` discountApplicableComponents,
              c /= TollChargesComponent,
              c /= TollVatComponent,
              c /= RideVatComponent,
              c /= CustomerCancellationChargeComponent,
              c /= ParkingChargeComponent
          ]
      parkingBase = fromMaybe 0 fareParams.parkingCharge
  (discAppTax, nonDiscAppTax, parkingTaxValue) <-
    case farePolicy.vatChargeConfig of
      Nothing -> pure (0, 0, 0)
      Just config -> case parseCodeValue config.value of
        Nothing -> do
          logWarning $ "Unable to parse vatCharge value: " <> config.value
          pure (0, 0, 0)
        Just parsed ->
          pure
            ( applyParsedValue discTaxableBase parsed,
              applyParsedValue nonDiscTaxableBase parsed,
              if parkingTaxable then applyParsedValue parkingBase parsed else 0
            )

  tollVatValue <- case farePolicy.tollTaxChargeConfig of
    Just config -> do
      tollVatAmount <- computeConfiguredCharge "tollTaxCharge" componentMap (Just config)
      pure $ if tollVatAmount > 0 then Just tollVatAmount else Nothing
    Nothing -> pure Nothing

  let hasVatConfig = isJust farePolicy.vatChargeConfig
      -- Total ride VAT now also includes parkingTax — it flows through
      -- govtCharges for historical reporting.
      totalRideVat = discAppTax + nonDiscAppTax + parkingTaxValue
      mergedGovtCharges = fromMaybe 0 fareParams.govtCharges + totalRideVat

      -- Canonical ten-slot fare partition.
      discAppTaxExcl = sum $ map (componentAmount componentMap) discountApplicableComponents
      tollExcl = fromMaybe 0 fareParams.tollCharges
      tollTax = fromMaybe 0 tollVatValue
      cancellationExcl = fromMaybe 0 fareParams.customerCancellationDues
      cancellationTaxV = 0
      parkingExcl = parkingBase
      parkingTax = parkingTaxValue

      -- Partially-updated fareParams carrying every field that's known at
      -- this point — only 'nonDiscountApplicableRideFareTaxExclusive'
      -- depends on 'postUpdateFareSum' and is filled in as the final
      -- record update below. The 10 partition slots are populated in
      -- EVERY mode (even pure GST with no VAT config, where the VAT
      -- subtotals are just 0) so downstream discount math works
      -- uniformly without branching on hasVatConfig.
      partiallyUpdatedFareParams =
        fareParams
          { paymentProcessingFee = Nothing,
            govtCharges = if mergedGovtCharges > 0 then Just mergedGovtCharges else fareParams.govtCharges,
            isVatTaxType = Just hasVatConfig,
            discountApplicableRideFareTaxExclusive = Just discAppTaxExcl,
            discountApplicableRideFareTax = Just discAppTax,
            nonDiscountApplicableRideFareTax = Just nonDiscAppTax,
            tollFareTaxExclusive = Just tollExcl,
            tollFareTax = Just tollTax,
            cancellationFeeTaxExclusive = Just cancellationExcl,
            cancellationTax = Just cancellationTaxV,
            parkingChargeTaxExclusive = Just parkingExcl,
            parkingChargeTax = Just parkingTax
          }
      postUpdateFareSum = pureFareSum partiallyUpdatedFareParams Nothing

      -- 'nonDiscountApplicableRideFareTaxExclusive' catches the residual —
      -- everything in pureFareSum that isn't in D / toll / cancellation /
      -- parking. Its corresponding tax slot is VAT computed on the subset
      -- (V \ D \ parking \ toll \ cancellation) only.
      accountedFor =
        discAppTaxExcl + discAppTax
          + tollExcl
          + tollTax
          + cancellationExcl
          + cancellationTaxV
          + parkingExcl
          + parkingTax
          + nonDiscAppTax
      nonDiscAppTaxExcl = max 0 (postUpdateFareSum - accountedFor)

  pure $
    partiallyUpdatedFareParams
      { nonDiscountApplicableRideFareTaxExclusive = Just nonDiscAppTaxExcl
      }

-- | Apply airport entry fee into parkingCharge, based on pickupGateId and transporter config.
applyAirportEntryFee ::
  (MonadFlow m, EsqDBFlow m r, BeamFlow m r) =>
  CalculateFareParametersParams ->
  FareParameters ->
  m FareParameters
applyAirportEntryFee params fareParams = case (params.merchantOperatingCityId, params.pickupGateId) of
  (Just merchantOperatingCityId, Just gateIdText) -> do
    transporterConfig <-
      SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing
        >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
    if not (fromMaybe False transporterConfig.airportEntryFeeEnabled)
      then pure fareParams
      else do
        airportFee <- entryFeeForGateId (Id gateIdText)
        let currentParking = fromMaybe 0 fareParams.parkingCharge
        pure $
          if airportFee > 0
            then fareParams {parkingCharge = Just (currentParking + airportFee)}
            else fareParams
  _ -> pure fareParams

-- | Entry fee for a single gate. Use when API sends gateId (e.g. SearchRequest/Booking.pickupGateId).
--   Returns 0 if gate not found or no fee configured.
entryFeeForGateId ::
  (Esq.EsqDBFlow m r, MonadFlow m) =>
  Id DGI.GateInfo ->
  m HighPrecMoney
entryFeeForGateId gateId = do
  mbGate <- QGI.findById gateId
  pure $ maybe 0 (fromMaybe 0 . fmap realToFrac . (.entryFeeAmount)) mbGate

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
            (PlatformFeeComponent, maybeZero platformFee),
            (TollVatComponent, maybeZero tollFareTax),
            (RideVatComponent, maybeZero discountApplicableRideFareTax)
          ]
      -- Detail map: Additional components based on fare policy type
      detailMap = case fareParametersDetails of
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
            [ (AmbulanceDistBasedFareComponent, det.distBasedFare)
            ]
        DFParams.SlabDetails _ ->
          Map.empty -- Slab details don't have additional components
          -- Merge base and detail maps (detail map takes precedence if key exists in both)
   in Map.union baseMap detailMap

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
calculateCommission ::
  MonadFlow m =>
  FareParameters ->
  Maybe FullFarePolicy ->
  m (Maybe HighPrecMoney)
calculateCommission fareParams mbFarePolicy = do
  case mbFarePolicy of
    Nothing -> pure Nothing
    Just farePolicy -> do
      let componentMap = buildComponentMap fareParams
      case farePolicy.commissionChargeConfig of
        Just config -> do
          commAmount <- computeConfiguredCharge "commissionCharge" componentMap (Just config)
          pure $ if commAmount > 0 then Just commAmount else Nothing
        Nothing -> pure Nothing

-- | Hardcoded list of 'FareChargeComponent' values the customer offer
--   discount is allowed to apply to. Toll (TollChargesComponent +
--   TollVatComponent) is handled separately; RideVatComponent is
--   folded into discountApplicableTax via fp.rideTax — so neither
--   belongs in this list.
--
--   This list is the single deterministic source of truth for
--   "what participates in the customer offer discount" on BPP: grep
--   here to answer that question without reading other modules.
--   Add a component here to make it discount-applicable; remove to
--   exclude it. Anything not in this list (parking, airport fee,
--   platform fee, card charges, insurance, cancellation dues,
--   payment processing fee, etc.) is treated as non-discount-applicable.
discountApplicableComponents :: [FareChargeComponent]
discountApplicableComponents =
  [ RideFare,
    WaitingCharge,
    ServiceChargeComponent,
    CongestionChargeComponent,
    PetChargeComponent,
    PriorityChargeComponent,
    NightShiftChargeComponent,
    StopChargeComponent,
    CustomerExtraFeeComponent,
    DeadKmFareComponent,
    ExtraKmFareComponent,
    RideDurationFareComponent,
    TimeBasedFareComponent,
    DistBasedFareComponent,
    TimeFareComponent,
    DistanceFareComponent,
    PickupChargeComponent,
    ExtraDistanceFareComponent,
    ExtraTimeFareComponent,
    AmbulanceDistBasedFareComponent
  ]

-- | Project FareParameters into the canonical eight-slot wire breakup.
--   Trivial record-to-record projection — the numbers are computed at
--   'calculateFareParameters' time and stored directly on the
--   'FareParameters' record. Returns 'Nothing' when those V2 slots
--   aren't populated (pure GST mode); BAP then falls back to
--   estimatedFare-based math.
projectFareParamsBreakup :: FareParameters -> Maybe RD.ProjectFareParamsBreakup
projectFareParamsBreakup FareParameters {..}
  | isNothing discountApplicableRideFareTaxExclusive
      && isNothing discountApplicableRideFareTax
      && isNothing nonDiscountApplicableRideFareTaxExclusive =
    Nothing
  | otherwise =
    Just
      RD.ProjectFareParamsBreakup
        { RD.discountApplicableRideFareTaxExclusive = fromMaybe 0 discountApplicableRideFareTaxExclusive,
          RD.discountApplicableRideFareTax = fromMaybe 0 discountApplicableRideFareTax,
          RD.nonDiscountApplicableRideFareTaxExclusive = fromMaybe 0 nonDiscountApplicableRideFareTaxExclusive,
          RD.nonDiscountApplicableRideFareTax = fromMaybe 0 nonDiscountApplicableRideFareTax,
          RD.tollFareTaxExclusive = fromMaybe 0 tollFareTaxExclusive,
          RD.tollFareTax = fromMaybe 0 tollFareTax,
          RD.cancellationFeeTaxExclusive = fromMaybe 0 cancellationFeeTaxExclusive,
          RD.cancellationTax = fromMaybe 0 cancellationTax,
          RD.parkingChargeTaxExclusive = fromMaybe 0 parkingChargeTaxExclusive,
          RD.parkingChargeTax = fromMaybe 0 parkingChargeTax
        }

-- | Clamp a raw (BAP-reported) discount amount to the discount-applicable
--   base. The base is derived from the hardcoded
--   'discountApplicableComponents' list via 'projectFareParamsBreakup'
--   (NOT from a legacy toll+parking aggregate subtraction) — toll,
--   parking, airport fees, platform fee, insurance, etc. are all
--   excluded by construction. Returns 'Nothing' when the projection is
--   unavailable (pure GST mode — discount can't be applied safely) or
--   when the clamped amount is zero.
clampDiscountToDiscountable :: FareParameters -> Maybe HighPrecMoney -> Maybe HighPrecMoney
clampDiscountToDiscountable fareParams mbRaw = do
  raw <- mbRaw
  b <- projectFareParamsBreakup fareParams
  let clamped = RD.clampDiscount b raw
  if clamped > 0 then Just clamped else Nothing
