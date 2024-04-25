{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareCalculator
  ( mkFareParamsBreakups,
    fareSum,
    pureFareSum,
    CalculateFareParametersParams (..),
    calculateFareParameters,
    isNightShift,
    timeZoneIST,
    UTCTime (UTCTime, utctDay),
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant hiding (NightShiftChargeAPIEntity (..), Variant (..), WaitingChargeAPIEntity (..))
import qualified Data.List.NonEmpty as NE
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.Merchant.TransporterConfig (AvgSpeedOfVechilePerKm)
import Domain.Types.ServiceTierType
import EulerHS.Prelude hiding (id, map)
import Kernel.Prelude
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

      waitingChargesCaption = show Enums.WAITING_OR_PICKUP_CHARGES
      mbWaitingChargesItem = mkBreakupItem waitingChargesCaption . mkPrice <$> fareParams.waitingCharge

      mbFixedGovtRateCaption = show Enums.FIXED_GOVERNMENT_RATE
      mbFixedGovtRateItem = mkBreakupItem mbFixedGovtRateCaption . mkPrice <$> fareParams.govtCharges

      customerCancellationDuesCaption = show Enums.CANCELLATION_CHARGES
      mbCustomerCancellationDues = mkBreakupItem customerCancellationDuesCaption . mkPrice <$> fareParams.customerCancellationDues

      tollChargesCaption = show Enums.TOLL_CHARGES
      mbTollChargesItem = mkBreakupItem tollChargesCaption . mkPrice <$> fareParams.tollCharges

      detailsBreakups = processFareParamsDetails dayPartRate fareParams.fareParametersDetails
  catMaybes
    [ Just baseFareItem,
      mbCongestionChargeItem,
      mbNightShiftChargeItem,
      mbWaitingChargesItem,
      mbFixedGovtRateItem,
      mbServiceChargeItem,
      mbSelectedFareItem,
      mkCustomerExtraFareItem,
      mkExtraTimeFareCaption,
      mbTollChargesItem,
      mbCustomerCancellationDues
    ]
    <> detailsBreakups
  where
    processFareParamsDetails dayPartRate = \case
      DFParams.ProgressiveDetails det -> mkFPProgressiveDetailsBreakupList dayPartRate det
      DFParams.SlabDetails det -> mkFPSlabDetailsBreakupList det
      DFParams.RentalDetails det -> mkFPRentalDetailsBreakupList det

    mkFPProgressiveDetailsBreakupList dayPartRate det = do
      let deadKmFareCaption = show Enums.DEAD_KILOMETER_FARE
          deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice det.deadKmFare)

          extraDistanceFareCaption = show Enums.DISTANCE_FARE
          mbExtraKmFare = det.extraKmFare <&> HighPrecMoney . (* toRational dayPartRate) . (.getHighPrecMoney) -- temp fix :: have to fix properly
          extraDistanceFareItem =
            mbExtraKmFare <&> \extraKmFareRounded ->
              mkBreakupItem extraDistanceFareCaption (mkPrice extraKmFareRounded)
      catMaybes [Just deadKmFareItem, extraDistanceFareItem]

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
      catMaybes [Just mbTimeBasedFare, Just mbDistBasedFare]

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
    + fromMaybe 0.0 fareParams.customerCancellationDues
    + fromMaybe 0.0 fareParams.tollCharges
    + partOfNightShiftCharge
    + notPartOfNightShiftCharge
    + platformFee

data CalculateFareParametersParams = CalculateFareParametersParams
  { farePolicy :: FullFarePolicy,
    actualDistance :: Maybe Meters,
    rideTime :: UTCTime,
    waitingTime :: Maybe Minutes,
    actualRideDuration :: Maybe Seconds,
    avgSpeedOfVehicle :: Maybe AvgSpeedOfVechilePerKm,
    driverSelectedFare :: Maybe HighPrecMoney,
    customerExtraFee :: Maybe HighPrecMoney,
    nightShiftCharge :: Maybe HighPrecMoney,
    customerCancellationDues :: Maybe HighPrecMoney,
    estimatedRideDuration :: Maybe Seconds,
    nightShiftOverlapChecking :: Bool,
    estimatedDistance :: Maybe Meters,
    timeDiffFromUtc :: Maybe Seconds,
    tollCharges :: Maybe HighPrecMoney,
    currency :: Currency
  }

calculateFareParameters ::
  (MonadFlow m) =>
  CalculateFareParametersParams ->
  m FareParameters
calculateFareParameters params = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| params.farePolicy.merchantId ||+ " and vehicle service tier " +|| params.farePolicy.vehicleServiceTier ||+ ""
  let fp = params.farePolicy
  id <- generateGUID
  now <- getCurrentTime
  let isNightShiftChargeIncluded = if params.nightShiftOverlapChecking then Just $ isNightAllowanceApplicable fp.nightShiftBounds params.rideTime now (fromMaybe 19800 params.timeDiffFromUtc) else isNightShift <$> fp.nightShiftBounds <*> Just params.rideTime
      (baseFare, nightShiftCharge, waitingChargeInfo, fareParametersDetails) = processFarePolicyDetails fp.farePolicyDetails
      (partOfNightShiftCharge, notPartOfNightShiftCharge, _) = countFullFareOfParamsDetails fareParametersDetails
      fullRideCost {-without govtCharges, serviceCharge, platformFee, waitingCharge, notPartOfNightShiftCharge and nightShift-} =
        baseFare
          + partOfNightShiftCharge
  let resultNightShiftCharge = (\isCoefIncluded -> if isCoefIncluded then countNightShiftCharge fullRideCost <$> nightShiftCharge else Nothing) =<< isNightShiftChargeIncluded
      resultWaitingCharge = countWaitingCharge =<< waitingChargeInfo
      congestionChargeResult = fp.congestionChargeMultiplier <&> \congestionCharge -> HighPrecMoney (fullRideCost.getHighPrecMoney * toRational congestionCharge) - fullRideCost
      fullRideCostN {-without govtCharges and platformFee-} =
        fullRideCost
          + fromMaybe 0.0 resultNightShiftCharge
          + fromMaybe 0.0 resultWaitingCharge
          + fromMaybe 0.0 congestionChargeResult
          + fromMaybe 0.0 fp.serviceCharge
          + notPartOfNightShiftCharge
      govtCharges =
        HighPrecMoney . (fullRideCostN.getHighPrecMoney *) . toRational <$> fp.govtCharges
      extraTimeFareInfo = calculateExtraTimeFare (fromMaybe 0 params.actualDistance) fp.perMinuteRideExtraTimeCharge params.actualRideDuration fp.vehicleServiceTier =<< params.avgSpeedOfVehicle -- todo tp transporter_config
      fullCompleteRideCost =
        {- without platformFee -}
        fullRideCostN
          + fromMaybe 0 govtCharges
      fareParams =
        FareParameters
          { id,
            driverSelectedFare = params.driverSelectedFare,
            customerExtraFee = params.customerExtraFee,
            serviceCharge = fp.serviceCharge,
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
              DFP.RentalDetails _ -> fareParametersDetails,
            customerCancellationDues = params.customerCancellationDues,
            tollCharges = params.tollCharges,
            updatedAt = now,
            currency = params.currency,
            ..
          }
  logTagInfo "FareCalculator" $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
  where
    processFarePolicyDetails = \case
      DFP.ProgressiveDetails det -> processFPProgressiveDetails det
      DFP.SlabsDetails det -> processFPSlabsDetailsSlab $ DFP.findFPSlabsDetailsSlabByDistance (fromMaybe 0 params.actualDistance) det.slabs
      DFP.RentalDetails det -> processFPRentalDetails det

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
      ( baseFare_,
        nightShiftCharge,
        Nothing,
        DFParams.RentalDetails $
          DFParams.FParamsRentalDetails
            { timeBasedFare = fareByTime,
              distBasedFare = fareByDist,
              extraDistance = Meters $ extraDist * 1000,
              extraDuration = Seconds $ extraMins * 60,
              currency = params.currency
            }
        )

    processFPProgressiveDetails DFP.FPProgressiveDetails {..} = do
      let mbExtraDistance =
            (fromMaybe 0 params.actualDistance) - baseDistance
              & (\dist -> if dist > 0 then Just dist else Nothing)
          mbExtraKmFare = processFPProgressiveDetailsPerExtraKmFare perExtraKmRateSections <$> mbExtraDistance
      ( baseFare,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.ProgressiveDetails $
          DFParams.FParamsProgressiveDetails
            { extraKmFare = mbExtraKmFare,
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

    processFPSlabsDetailsSlab DFP.FPSlabsDetailsSlab {..} = do
      ( baseFare,
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
      (DFParams.SlabDetails det) ->
        DFParams.SlabDetails $ maybe (FParamsSlabDetails Nothing Nothing Nothing det.currency) countPlatformFeeMath platformFeeInfo
      where
        countPlatformFeeMath platformFeeInfo' = do
          let baseFee = case platformFeeInfo'.platformFeeCharge of
                ProgressivePlatformFee charge -> fullCompleteRideCost * charge
                ConstantPlatformFee charge -> charge
          FParamsSlabDetails
            { platformFee = Just baseFee,
              cgst = Just . HighPrecMoney . toRational $ platformFeeInfo'.cgst * realToFrac baseFee,
              sgst = Just . HighPrecMoney . toRational $ platformFeeInfo'.sgst * realToFrac baseFee,
              currency
            }
    calculateExtraTimeFare :: Meters -> Maybe HighPrecMoney -> Maybe Seconds -> ServiceTierType -> AvgSpeedOfVechilePerKm -> Maybe HighPrecMoney
    calculateExtraTimeFare distance perMinuteRideExtraTimeCharge actualRideDuration serviceTier avgSpeedOfVehicle = do
      let actualRideDurationInMinutes = secondsToMinutes <$> actualRideDuration
      let avgSpeedOfVehicle' = realToFrac @_ @Double case serviceTier of
            SEDAN -> avgSpeedOfVehicle.sedan.getKilometers
            SUV -> avgSpeedOfVehicle.suv.getKilometers
            HATCHBACK -> avgSpeedOfVehicle.hatchback.getKilometers
            AUTO_RICKSHAW -> avgSpeedOfVehicle.autorickshaw.getKilometers
            TAXI -> avgSpeedOfVehicle.taxi.getKilometers
            TAXI_PLUS -> avgSpeedOfVehicle.taxiplus.getKilometers
            ECO -> avgSpeedOfVehicle.hatchback.getKilometers
            COMFY -> avgSpeedOfVehicle.sedan.getKilometers
            PREMIUM -> avgSpeedOfVehicle.sedan.getKilometers
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

countFullFareOfParamsDetails :: DFParams.FareParametersDetails -> (HighPrecMoney, HighPrecMoney, HighPrecMoney)
countFullFareOfParamsDetails = \case
  DFParams.ProgressiveDetails det -> (fromMaybe 0.0 det.extraKmFare, det.deadKmFare, 0.0) -- (partOfNightShiftCharge, notPartOfNightShiftCharge)
  DFParams.SlabDetails det -> (0.0, 0.0, fromMaybe 0.0 det.platformFee + fromMaybe 0.0 det.sgst + fromMaybe 0.0 det.cgst)
  DFParams.RentalDetails det -> (0.0, det.distBasedFare + det.timeBasedFare, 0.0)

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
isNightAllowanceApplicable nightShiftBounds tripStartTime now timeDiffFromUtc = do
  let localRideEndDate = utctDay $ addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
      localTripStartTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) tripStartTime
      localRideEndTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
  case nightShiftBounds of
    Nothing -> False
    Just bounds -> do
      let nightShiftStartTime = timeOfDayToDiffTime bounds.nightShiftStart
          nightShiftEndTime = timeOfDayToDiffTime bounds.nightShiftEnd
      if nightShiftStartTime <= 6 * 60 * 60 -- NS starting and ending on same date
        then isNightShiftOverlap localRideEndDate nightShiftStartTime nightShiftEndTime localTripStartTime localRideEndTime 0 0
        else isNightShiftOverlap localRideEndDate nightShiftStartTime nightShiftEndTime localTripStartTime localRideEndTime (-1) 0 || isNightShiftOverlap localRideEndDate nightShiftStartTime nightShiftEndTime localRideEndTime localRideEndTime 0 1

isNightShiftOverlap :: Day -> DiffTime -> DiffTime -> UTCTime -> UTCTime -> Integer -> Integer -> Bool
isNightShiftOverlap rideEndDate nightShiftStartTime nightShiftEndTime localTripStartTime localRideEndTime startAdd endAdd = do
  let curNightShiftStartTs = UTCTime (addDays startAdd rideEndDate) nightShiftStartTime
      curNightShiftEndTs = UTCTime (addDays endAdd rideEndDate) nightShiftEndTime
      curMxStart = max curNightShiftStartTs localTripStartTime
      curMnEnd = min curNightShiftEndTs localRideEndTime
  curMnEnd >= curMxStart

timeOfDayToDiffTime :: TimeOfDay -> DiffTime
timeOfDayToDiffTime (TimeOfDay h m s) = secondsToDiffTime $ fromIntegral (h * 3600 + m * 60 + floor s)
