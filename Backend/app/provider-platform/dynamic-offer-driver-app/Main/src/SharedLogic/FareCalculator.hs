{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE BangPatterns #-}

module SharedLogic.FareCalculator
  ( mkFareParamsBreakups,
    fareSum,
    pureFareSum,
    perRideKmFareParamsSum,
    getPerMinuteRate,
    CalculateFareParametersParams (..),
    calculateFareParameters,
    isNightShift,
    isNightAllowanceApplicable,
    timeZoneIST,
    UTCTime (UTCTime, utctDay),
    calculateCancellationCharges,
    calculateNoShowCharges,
  )
where

import "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant hiding (NightShiftChargeAPIEntity (..), VehicleVariant (..), WaitingChargeAPIEntity (..))
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.List.NonEmpty as NE
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Domain.Types.CancellationFarePolicy as DTCFP
import Domain.Types.Common
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as DFP
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.TransporterConfig (AvgSpeedOfVechilePerKm)
import EulerHS.Prelude hiding (id, map, sum)
import Kernel.Prelude as KP
import Kernel.Types.Id (Id)
import qualified Kernel.Types.Price as Price
import Kernel.Utils.Common hiding (isTimeWithinBounds, mkPrice)

mkFareParamsBreakups :: (HighPrecMoney -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkFareParamsBreakups mkPrice mkBreakupItem fareParams = do
  let dayPartRate = fromMaybe 1.0 fareParams.nightShiftRateIfApplies -- Temp fix :: have to fix properly
      baseFareFinal = HighPrecMoney $ fareParams.baseFare.getHighPrecMoney * toRational dayPartRate -- Temp fix :: have to fix properly
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

      parkingChargeCaption = show Enums.PARKING_CHARGE
      mbParkingChargeItem = mkBreakupItem parkingChargeCaption . mkPrice <$> fareParams.parkingCharge

      waitingChargesCaption = show Enums.WAITING_OR_PICKUP_CHARGES
      mbWaitingChargesItem = mkBreakupItem waitingChargesCaption . mkPrice <$> fareParams.waitingCharge

      mbFixedGovtRateCaption = show Enums.FIXED_GOVERNMENT_RATE
      mbFixedGovtRateItem = mkBreakupItem mbFixedGovtRateCaption . mkPrice <$> fareParams.govtCharges

      customerCancellationDuesCaption = show Enums.CANCELLATION_CHARGES
      mbCustomerCancellationDues = mkBreakupItem customerCancellationDuesCaption . mkPrice <$> fareParams.customerCancellationDues

      tollChargesCaption = show Enums.TOLL_CHARGES
      mbTollChargesItem = mkBreakupItem tollChargesCaption . mkPrice <$> fareParams.tollCharges

      insuranceChargeCaption = show Enums.INSURANCE_CHARGES
      mbInsuranceChargeItem = mkBreakupItem insuranceChargeCaption . mkPrice <$> fareParams.insuranceCharge

      cardChargesFareCaption = show Enums.CARD_CHARGES_ON_FARE
      mbCardChargesFareItem = fareParams.cardCharge >>= \cardCharge -> mkBreakupItem cardChargesFareCaption . mkPrice <$> cardCharge.onFare

      cardChargesFixedCaption = show Enums.CARD_CHARGES_FIXED
      mbCardChargesFixedItem = fareParams.cardCharge >>= \cardCharge -> mkBreakupItem cardChargesFixedCaption . mkPrice <$> cardCharge.fixed

      detailsBreakups = processFareParamsDetails dayPartRate fareParams.fareParametersDetails
  catMaybes
    [ Just baseFareItem,
      mbCongestionChargeItem,
      mbNightShiftChargeItem,
      mbParkingChargeItem,
      mbWaitingChargesItem,
      mbFixedGovtRateItem,
      mbServiceChargeItem,
      mbSelectedFareItem,
      mkCustomerExtraFareItem,
      mkExtraTimeFareCaption,
      mbTollChargesItem,
      mbCustomerCancellationDues,
      mbInsuranceChargeItem,
      mbCardChargesFareItem,
      mbCardChargesFixedItem
    ]
    <> detailsBreakups
  where
    processFareParamsDetails dayPartRate = \case
      DFParams.ProgressiveDetails det -> mkFPProgressiveDetailsBreakupList dayPartRate det
      DFParams.SlabDetails det -> mkFPSlabDetailsBreakupList det
      DFParams.RentalDetails det -> mkFPRentalDetailsBreakupList det
      DFParams.InterCityDetails det -> mkFPInterCityDetailsBreakupList det
      DFParams.AmbulanceDetails det -> mkFPAmbulanceDetailsBreakupList det

    mkFPProgressiveDetailsBreakupList dayPartRate det = do
      let deadKmFareCaption = show Enums.DEAD_KILOMETER_FARE
          deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice det.deadKmFare)

          extraDistanceFareCaption = show Enums.DISTANCE_FARE
          mbExtraKmFare = det.extraKmFare <&> HighPrecMoney . (* toRational dayPartRate) . (.getHighPrecMoney) -- temp fix :: have to fix properly
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

fareSum :: FareParameters -> HighPrecMoney
fareSum fareParams = do
  pureFareSum fareParams
    + fromMaybe 0.0 fareParams.driverSelectedFare
    + fromMaybe 0.0 fareParams.customerExtraFee

-- Pure fare without customerExtraFee and driverSelectedFare
pureFareSum :: FareParameters -> HighPrecMoney
pureFareSum fareParams = do
  let (partOfNightShiftCharge, notPartOfNightShiftCharge, platformFee) = countFullFareOfParamsDetails fareParams.fareParametersDetails
  fareParams.baseFare
    + fromMaybe 0.0 fareParams.serviceCharge
    + fromMaybe 0.0 fareParams.waitingCharge
    + fromMaybe 0.0 fareParams.govtCharges
    + fromMaybe 0.0 fareParams.nightShiftCharge
    + fromMaybe 0.0 fareParams.rideExtraTimeFare
    + fromMaybe 0.0 fareParams.congestionCharge
    + fromMaybe 0.0 fareParams.stopCharges
    + partOfNightShiftCharge
    + notPartOfNightShiftCharge
    + platformFee
    + (fromMaybe 0.0 fareParams.customerCancellationDues + fromMaybe 0.0 fareParams.tollCharges + fromMaybe 0.0 fareParams.parkingCharge)
    + fromMaybe 0.0 fareParams.insuranceCharge
    + fromMaybe 0.0 (fareParams.cardCharge >>= (.onFare))
    + fromMaybe 0.0 (fareParams.cardCharge >>= (.fixed))

perRideKmFareParamsSum :: FareParameters -> HighPrecMoney
perRideKmFareParamsSum fareParams = do
  let (partOfNightShiftCharge, notPartOfNightShiftCharge, _) = countFullFareOfParamsDetails fareParams.fareParametersDetails
  fareParams.baseFare
    + partOfNightShiftCharge
    + notPartOfNightShiftCharge
    + fromMaybe 0.0 fareParams.nightShiftCharge
    + fromMaybe 0.0 fareParams.congestionCharge

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
    avgSpeedOfVehicle :: Maybe AvgSpeedOfVechilePerKm,
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
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity)
  }

calculateFareParameters :: MonadFlow m => CalculateFareParametersParams -> m FareParameters
calculateFareParameters params = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| params.farePolicy.merchantId ||+ " and vehicle service tier " +|| params.farePolicy.vehicleServiceTier ||+ ""
  now <- getCurrentTime
  let fp = params.farePolicy
      rideEndTime = case (params.actualRideDuration, params.estimatedRideDuration) of
        (Just duration, _) -> addUTCTime (secondsToNominalDiffTime duration) params.rideTime
        (_, Just duration) -> addUTCTime (secondsToNominalDiffTime duration) params.rideTime
        _ -> now
  id <- generateGUID
  let localTimeZoneSeconds = fromMaybe 19800 params.timeDiffFromUtc
  let isNightShiftChargeIncluded = if params.nightShiftOverlapChecking then Just $ isNightAllowanceApplicable fp.nightShiftBounds params.rideTime rideEndTime localTimeZoneSeconds else isNightShift <$> fp.nightShiftBounds <*> Just params.rideTime
      (debugLogs, baseFare, nightShiftCharge, waitingChargeInfo, fareParametersDetails) = processFarePolicyDetails fp.farePolicyDetails
      (partOfNightShiftCharge, notPartOfNightShiftCharge, _) = countFullFareOfParamsDetails fareParametersDetails
      fullRideCost {-without govtCharges, serviceCharge, platformFee, waitingCharge, notPartOfNightShiftCharge, nightShift, insuranceCharge, cardChargeOnFare and fixedCardCharge-} =
        baseFare
          + partOfNightShiftCharge
  let resultNightShiftCharge = (\isCoefIncluded -> if isCoefIncluded then countNightShiftCharge fullRideCost <$> nightShiftCharge else Nothing) =<< isNightShiftChargeIncluded
      resultWaitingCharge = countWaitingCharge =<< waitingChargeInfo
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
      fullRideCostN {-without govtCharges, platformFee, cardChargeOnFare and fixedCharge-} =
        fullRideCost
          + fromMaybe 0.0 resultNightShiftCharge
          + fromMaybe 0.0 resultWaitingCharge
          + finalCongestionCharge ----------Needs to be changed to congestionChargeResult
          + fromMaybe 0.0 fp.serviceCharge
          + fromMaybe 0.0 insuranceChargeResult
          + notPartOfNightShiftCharge
      govtCharges =
        HighPrecMoney . (fullRideCostN.getHighPrecMoney *) . toRational <$> fp.govtCharges
      stopCharges =
        HighPrecMoney . ((toRational params.noOfStops) *) . toRational <$> fp.perStopCharge
      extraTimeFareInfo = calculateExtraTimeFare (fromMaybe 0 params.actualDistance) fp.perMinuteRideExtraTimeCharge params.actualRideDuration fp.vehicleServiceTier =<< params.avgSpeedOfVehicle -- todo tp transporter_config
      fullCompleteRideCost =
        {- without platformFee -}
        fullRideCostN
          + fromMaybe 0 govtCharges
      cardChargeOnFare = countCardChargeOnFare fullCompleteRideCost <$> (fp.cardCharge >>= (.perDistanceUnitMultiplier))
      fareParams =
        FareParameters
          { id,
            driverSelectedFare = params.driverSelectedFare,
            customerExtraFee = params.customerExtraFee,
            serviceCharge = fp.serviceCharge,
            parkingCharge = fp.parkingCharge,
            congestionCharge = Just finalCongestionCharge,
            congestionChargeViaDp = congestionChargeByPerMin,
            stopCharges = stopCharges, --(\charges -> Just $ HighPrecMoney (toRational params.noOfStops * charges))=<< fp.perStopCharge,
            waitingCharge = resultWaitingCharge,
            nightShiftCharge = resultNightShiftCharge,
            rideExtraTimeFare = extraTimeFareInfo,
            nightShiftRateIfApplies = (\isCoefIncluded -> if isCoefIncluded then getNightShiftRate nightShiftCharge else Nothing) =<< isNightShiftChargeIncluded, -- Temp fix :: have to fix properly
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
            insuranceCharge = insuranceChargeResult,
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
            ..
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

      let estimatedDistance = (.getMeters) <$> params.estimatedDistance
          estimatedDistanceInKm = max (estimatedDurationInHr * includedKmPerHr.getKilometers) (fromMaybe 0 estimatedDistance `div` 1000)
          actualDistance = (.getMeters) <$> params.actualDistance
          extraDist = max 0 (fromMaybe 0 actualDistance - fromMaybe 0 estimatedDistance)
          distanceBuffer = DFP.findFPRentalDetailsByDuration actualRideDurationInHr distanceBuffers
          fareByDist = if extraDist > distanceBuffer.bufferMeters then HighPrecMoney (toRational (fromIntegral extraDist / 1000 :: Double) * perExtraKmRate.getHighPrecMoney) else 0

      let extraPlannedKm = max 0 (estimatedDistanceInKm - (estimatedDurationInHr * includedKmPerHr.getKilometers))
          extraPlannedKmFare = toRational extraPlannedKm * plannedPerKmRate.getHighPrecMoney
          potentialBaseFare = toRational estimatedDurationInHr * perHourCharge.getHighPrecMoney + extraPlannedKmFare

      let distPercent = case (actualDistance, estimatedDistance) of
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
              extraDistance = Meters extraDist,
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

    calculateExtraTimeFare :: Meters -> Maybe HighPrecMoney -> Maybe Seconds -> ServiceTierType -> AvgSpeedOfVechilePerKm -> Maybe HighPrecMoney
    calculateExtraTimeFare distance perMinuteRideExtraTimeCharge actualRideDuration serviceTier avgSpeedOfVehicle = do
      let actualRideDurationInMinutes = secondsToMinutes <$> actualRideDuration
      let avgSpeedOfVehicle' = realToFrac @_ @Double case serviceTier of
            SEDAN -> avgSpeedOfVehicle.sedan.getKilometers
            SUV -> avgSpeedOfVehicle.suv.getKilometers
            HATCHBACK -> avgSpeedOfVehicle.hatchback.getKilometers
            AUTO_RICKSHAW -> avgSpeedOfVehicle.autorickshaw.getKilometers
            BIKE -> avgSpeedOfVehicle.bike.getKilometers
            DELIVERY_BIKE -> avgSpeedOfVehicle.bike.getKilometers
            TAXI -> avgSpeedOfVehicle.taxi.getKilometers
            TAXI_PLUS -> avgSpeedOfVehicle.taxiplus.getKilometers
            PREMIUM_SEDAN -> avgSpeedOfVehicle.premiumsedan.getKilometers
            BLACK -> avgSpeedOfVehicle.black.getKilometers
            BLACK_XL -> avgSpeedOfVehicle.blackxl.getKilometers
            ECO -> avgSpeedOfVehicle.hatchback.getKilometers
            COMFY -> avgSpeedOfVehicle.sedan.getKilometers
            PREMIUM -> avgSpeedOfVehicle.sedan.getKilometers
            AMBULANCE_TAXI -> avgSpeedOfVehicle.ambulance.getKilometers
            AMBULANCE_TAXI_OXY -> avgSpeedOfVehicle.ambulance.getKilometers
            AMBULANCE_AC -> avgSpeedOfVehicle.ambulance.getKilometers
            AMBULANCE_AC_OXY -> avgSpeedOfVehicle.ambulance.getKilometers
            AMBULANCE_VENTILATOR -> avgSpeedOfVehicle.ambulance.getKilometers
            SUV_PLUS -> avgSpeedOfVehicle.suvplus.getKilometers
            HERITAGE_CAB -> avgSpeedOfVehicle.heritagecab.getKilometers
            EV_AUTO_RICKSHAW -> avgSpeedOfVehicle.evautorickshaw.getKilometers
            DELIVERY_LIGHT_GOODS_VEHICLE -> avgSpeedOfVehicle.deliveryLightGoodsVehicle.getKilometers
            DELIVERY_TRUCK_MINI -> avgSpeedOfVehicle.deliveryLightGoodsVehicle.getKilometers
            DELIVERY_TRUCK_SMALL -> avgSpeedOfVehicle.deliveryLightGoodsVehicle.getKilometers
            DELIVERY_TRUCK_MEDIUM -> avgSpeedOfVehicle.deliveryLightGoodsVehicle.getKilometers
            DELIVERY_TRUCK_LARGE -> avgSpeedOfVehicle.deliveryLightGoodsVehicle.getKilometers
            DELIVERY_TRUCK_ULTRA_LARGE -> avgSpeedOfVehicle.deliveryLightGoodsVehicle.getKilometers
            BUS_NON_AC -> avgSpeedOfVehicle.busNonAc.getKilometers
            BUS_AC -> avgSpeedOfVehicle.busAc.getKilometers
      if avgSpeedOfVehicle' > 0
        then do
          let distanceInKilometer = realToFrac @_ @Double distance.getMeters / 1000
          let perMinuteRideExtraTimeCharge' = fromMaybe 0.0 perMinuteRideExtraTimeCharge
          let estimatedTimeTakeInMinutes :: Int = round $ (distanceInKilometer / avgSpeedOfVehicle') * 60
          let rideDurationDifference = realToFrac @_ @Double <$> (\actualRideDurationInMinutes' -> actualRideDurationInMinutes' - estimatedTimeTakeInMinutes) <$> (actualRideDurationInMinutes <&> getMinutes)
          let extraTimeFare = HighPrecMoney . (* perMinuteRideExtraTimeCharge'.getHighPrecMoney) . toRational <$> rideDurationDifference
          case extraTimeFare of
            Just fare | fare > 0 -> Just fare
            _ -> Nothing
        else Nothing
    countInsuranceChargeForDistance :: DistanceUnit -> Maybe Meters -> Maybe HighPrecMoney -> Maybe HighPrecMoney
    countInsuranceChargeForDistance dUnit mbDistance mbChargePerUnit =
      liftM2 (,) mbDistance mbChargePerUnit
        <&> \(distanceInMtrs, chargePerUnit) ->
          let distance = convertMetersToDistance dUnit distanceInMtrs
           in HighPrecMoney $ distance.value.getHighPrecDistance * chargePerUnit.getHighPrecMoney
    countCardChargeOnFare :: HighPrecMoney -> Double -> HighPrecMoney
    countCardChargeOnFare fullCompleteRideCost cardCharge =
      HighPrecMoney (fullCompleteRideCost.getHighPrecMoney * toRational (max 1 cardCharge)) - fullCompleteRideCost

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

isNightShift ::
  NightShiftBounds ->
  UTCTime ->
  Bool
isNightShift nightShiftBounds time = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST time
  let nightShiftStart = nightShiftBounds.nightShiftStart
  let nightShiftEnd = nightShiftBounds.nightShiftEnd
  isTimeWithinBounds nightShiftStart nightShiftEnd timeOfDay

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
