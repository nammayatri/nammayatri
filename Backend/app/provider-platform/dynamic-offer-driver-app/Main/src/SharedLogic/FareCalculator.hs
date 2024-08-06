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

import qualified BecknV2.OnDemand.Enums as Enums
import "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Merchant hiding (NightShiftChargeAPIEntity (..), Variant (..), WaitingChargeAPIEntity (..))
import qualified Data.List.NonEmpty as NE
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Domain.Types.CancellationFarePolicy as DTCFP
import Domain.Types.Common
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.ServiceTierType
import Domain.Types.TransporterConfig (AvgSpeedOfVechilePerKm)
import EulerHS.Prelude hiding (id, map, sum)
import Kernel.Prelude as KP
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
              mkBreakupItem extraDistanceFareCaption (mkPrice (extraKmFareRounded + fromMaybe 0.0 fareParams.congestionCharge)) -- temp fix :: MOVE CONGESTION CHARGE TO PROPER PLACE
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

      catMaybes [Just deadKmFareItem, Just mbTimeBasedFare, Just mbDistBasedFare, Just extraDistanceFareItem, Just extraTimeFareItem]

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
    rideDurationForFareCalc :: Maybe Minutes,
    nightShiftOverlapChecking :: Bool,
    estimatedDistance :: Maybe Meters,
    timeDiffFromUtc :: Maybe Seconds,
    tollCharges :: Maybe HighPrecMoney,
    currency :: Currency,
    distanceUnit :: DistanceUnit
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
      congestionChargeResult =
        fp.congestionChargeMultiplier <&> \case
          DFP.BaseFareAndExtraDistanceFare congestionCharge -> HighPrecMoney (fullRideCost.getHighPrecMoney * toRational congestionCharge) - fullRideCost
          DFP.ExtraDistanceFare congestionCharge -> HighPrecMoney (partOfNightShiftCharge.getHighPrecMoney * toRational congestionCharge) - partOfNightShiftCharge
      insuranceChargeResult = countInsuranceChargeForDistance fp.distanceUnit params.actualDistance fp.perDistanceUnitInsuranceCharge
      fullRideCostN {-without govtCharges, platformFee, cardChargeOnFare and fixedCharge-} =
        fullRideCost
          + fromMaybe 0.0 resultNightShiftCharge
          + fromMaybe 0.0 resultWaitingCharge
          + fromMaybe 0.0 congestionChargeResult
          + fromMaybe 0.0 fp.serviceCharge
          + fromMaybe 0.0 insuranceChargeResult
          + notPartOfNightShiftCharge
      govtCharges =
        HighPrecMoney . (fullRideCostN.getHighPrecMoney *) . toRational <$> fp.govtCharges
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
            congestionCharge = congestionChargeResult,
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
      let estimatedDistance = maybe 0 (.getMeters) params.estimatedDistance
          actualDistance = (.getMeters) <$> params.actualDistance
          distanceInKm = (fromIntegral $ fromMaybe estimatedDistance actualDistance) / 1000
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
          actualDuration = maybe estimatedDuration (.getSeconds) params.actualRideDuration
          estimatedDurationInHr = estimatedDuration `div` 3600
          perDayMaxHourAllowanceInHr = perDayMaxHourAllowance.getHours
          allowanceHours' = maybe 0 (\rt -> (calculateAllowanceHours (fromMaybe 19800 params.timeDiffFromUtc) perDayMaxHourAllowanceInHr params.rideTime rt) - estimatedDurationInHr) params.returnTime
          defaultWaitTimeAtDestinationInHrs = defaultWaitTimeAtDestination.getMinutes `div` 60
          allowanceHours = if params.roundTrip then max defaultWaitTimeAtDestinationInHrs allowanceHours' else 0
          extraMins = max 0 (actualDuration - estimatedDuration) `div` 60
          extraTimeFare = HighPrecMoney $ toRational extraMins * perExtraMinRate.getHighPrecMoney
          fareByTime = HighPrecMoney $ toRational (estimatedDurationInHr + allowanceHours) * perHourCharge.getHighPrecMoney

      let perKmRate = if params.roundTrip then perKmRateRoundTrip else perKmRateOneWay
          estimatedDistance = maybe 0 (.getMeters) params.estimatedDistance
          estimatedDistanceInKm = estimatedDistance `div` 1000
          actualDistance = (.getMeters) <$> params.actualDistance
          actualDistanceInKm = fromMaybe estimatedDistanceInKm actualDistance `div` 1000
          extraDist = max 0 (actualDistanceInKm - estimatedDistanceInKm)
          extraDistanceFare = HighPrecMoney $ toRational extraDist * perExtraKmRate.getHighPrecMoney
          fareByDist = HighPrecMoney $ toRational ((max 0 (allowanceHours - defaultWaitTimeAtDestinationInHrs) * kmPerPlannedExtraHour.getKilometers) + estimatedDistanceInKm) * perKmRate.getHighPrecMoney

      ( [],
        baseFare,
        nightShiftCharge,
        Nothing,
        DFParams.InterCityDetails $
          DFParams.FParamsInterCityDetails
            { timeFare = fareByTime,
              distanceFare = fareByDist,
              pickupCharge = deadKmFare,
              extraDistanceFare,
              extraTimeFare,
              currency = params.currency
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
          actualDistanceInKm = fromMaybe estimatedDistanceInKm actualDistance `div` 1000
          extraDist = max 0 (actualDistanceInKm - estimatedDistanceInKm)
          distanceBuffer = DFP.findFPRentalDetailsByDuration actualRideDurationInHr distanceBuffers
          fareByDist = if extraDist > distanceBuffer.bufferKms then HighPrecMoney (toRational extraDist * perExtraKmRate.getHighPrecMoney) else 0

      let extraPlannedKm = max 0 (estimatedDistanceInKm - (estimatedDurationInHr * includedKmPerHr.getKilometers))
          extraPlannedKmFare = HighPrecMoney $ toRational extraPlannedKm * plannedPerKmRate.getHighPrecMoney
          baseFare_ = HighPrecMoney (toRational estimatedDurationInHr * perHourCharge.getHighPrecMoney) + extraPlannedKmFare
      ( [],
        baseFare_,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.RentalDetails $
          DFParams.FParamsRentalDetails
            { timeBasedFare = fareByTime,
              distBasedFare = fareByDist,
              extraDistance = Meters $ extraDist * 1000,
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
          mbRideDuration = params.rideDurationForFareCalc
          mbExtraKmFare = processFPProgressiveDetailsPerExtraKmFare perExtraKmRateSections <$> mbExtraDistance
          (mbRideDurationFare, debugLogs) = maybe (Nothing, []) (processFPProgressiveDetailsPerMinuteFare perMinRateSections) mbRideDuration
      ( debugLogs,
        baseFare,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.ProgressiveDetails $
          DFParams.FParamsProgressiveDetails
            { extraKmFare = mbExtraKmFare,
              rideDurationFare = mbRideDurationFare,
              ..
            }
        )

    processFPProgressiveDetailsPerExtraKmFare perExtraKmRateSections (extraDistance :: Meters) = do
      let sortedPerExtraKmFareSections = NE.sortBy (comparing (.startDistance)) perExtraKmRateSections
      processFPProgressiveDetailsPerExtraKmFare' sortedPerExtraKmFareSections extraDistance
      where
        processFPProgressiveDetailsPerExtraKmFare' _ 0 = 0 :: HighPrecMoney
        processFPProgressiveDetailsPerExtraKmFare' sortedPerExtraKmFareSectionsLeft (extraDistanceLeft :: Meters) =
          case sortedPerExtraKmFareSectionsLeft of
            aSection :| [] -> HighPrecMoney $ toRational extraDistanceLeft * (getPerExtraMRate aSection.perExtraKmRate).getHighPrecMoney
            aSection :| bSection : leftSections -> do
              let sectionDistance = bSection.startDistance - aSection.startDistance
                  extraDistanceWithinSection = min sectionDistance extraDistanceLeft
              HighPrecMoney (toRational extraDistanceWithinSection * (getPerExtraMRate aSection.perExtraKmRate).getHighPrecMoney)
                + processFPProgressiveDetailsPerExtraKmFare' (bSection :| leftSections) (extraDistanceLeft - extraDistanceWithinSection)
        getPerExtraMRate perExtraKmRate = perExtraKmRate / 1000

    processFPProgressiveDetailsPerMinuteFare mbPerMinRateSections rideDuration =
      case mbPerMinRateSections of
        Nothing -> (Nothing, ["No per min rate sections configured"])
        Just perMinRateSections -> do
          let sortedPerMinFareSections = NE.sortBy (comparing (.rideDuration)) perMinRateSections
              fpDebugLogStr = "Sorted per min rate sections: " +|| NE.toList sortedPerMinFareSections ||+ " for ride duration (in mins): " +|| rideDuration ||+ ""

          let rideDurationFare = HighPrecMoney $ processFPPDPerMinFare rideDuration sortedPerMinFareSections
              rideFareDebugLogStr = "Ride duration fare: " +|| rideDurationFare ||+ ""

          (Just rideDurationFare, [fpDebugLogStr, rideFareDebugLogStr])
      where
        {- Sections for e.g.: (0, 5], (5, 15], (15, 30] -}
        processFPPDPerMinFare (0 :: Minutes) _ = 0.0 :: Rational
        processFPPDPerMinFare !remRideDuration (aSection :| []) = toRational remRideDuration * aSection.perMinRate.amount.getHighPrecMoney
        processFPPDPerMinFare !remRideDuration (aSection :| bSection : remSections) =
          let sectionDuration = bSection.rideDuration - aSection.rideDuration
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
      let waitingTimeMinusFreeWatingTime = params.waitingTime <&> (\wt -> (-) wt waitingChargeInfo.freeWaitingTime)
      let chargedWaitingTime = if waitingTimeMinusFreeWatingTime < Just 0 then Nothing else waitingTimeMinusFreeWatingTime
      case waitingChargeInfo.waitingCharge of
        PerMinuteWaitingCharge charge -> (\waitingTime -> HighPrecMoney $ toRational waitingTime * charge.getHighPrecMoney) <$> chargedWaitingTime
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
  DFParams.InterCityDetails det -> (0.0, det.pickupCharge + det.distanceFare + det.timeFare + det.extraDistanceFare + det.extraTimeFare, 0.0)
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

-- Calculates hours for a partial day
hoursInDay :: UTCTime -> UTCTime -> LocalTime -> LocalTime -> Day -> Int
hoursInDay startUtc endUtc start end day
  | startDay == endDay = if startDay == day then diffHours else 0
  | otherwise =
    if day == startDay
      then 24 - todHour startTod
      else
        if day == endDay
          then todHour endTod
          else 24
  where
    startDay = localDay start
    endDay = localDay end
    startTod = localTimeOfDay start
    endTod = localTimeOfDay end
    diffSeconds = nominalDiffTimeToSeconds $ diffUTCTime endUtc startUtc
    diffHours = diffSeconds.getSeconds `div` 3600

-- Main function to calculate total hours with 14-hour daily cap
calculateAllowanceHours :: Seconds -> Int -> UTCTime -> UTCTime -> Int
calculateAllowanceHours timeDiffFromUtc perDayMaxHourAllowance startTime endTime =
  sum $ map (min perDayMaxHourAllowance . hoursInDay startTime endTime startIST endIST) [startDay .. endDay]
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
