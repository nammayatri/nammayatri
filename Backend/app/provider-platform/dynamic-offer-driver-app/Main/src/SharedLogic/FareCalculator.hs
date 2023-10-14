{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module SharedLogic.FareCalculator
  ( mkBreakupList,
    fareSum,
    pureFareSum,
    CalculateFareParametersParams (..),
    calculateFareParameters,
    isNightShift,
    timeZoneIST,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (..),
    TimeZone,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
-- import Domain.Types.FarePolicy.FarePolicyRentalSlabsDetails
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Utils.Common

-- import Kernel.Utils.Common (Kilometers(getKilometers), Meters (getMeters))
-- import Extra (intToFloat)

mkBreakupList :: (Money -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkBreakupList mkPrice mkBreakupItem fareParams = do
  let dayPartRate = fromMaybe 1.0 fareParams.nightShiftRateIfApplies -- Temp fix :: have to fix properly
      baseFareFinalRounded = roundToIntegral $ fromIntegral fareParams.baseFare * dayPartRate -- Temp fix :: have to fix properly
      baseFareCaption = "BASE_FARE"
      baseFareItem = mkBreakupItem baseFareCaption (mkPrice baseFareFinalRounded)
      baseFareDistanceCaption = "BASE_DISTANCE_FARE" --TODO: deprecated, to be removed
      baseFareDistanceItem = mkBreakupItem baseFareDistanceCaption (mkPrice fareParams.baseFare)

      serviceChargeCaption = "SERVICE_CHARGE"
      mbServiceChargeItem = fmap (mkBreakupItem serviceChargeCaption) (mkPrice <$> fareParams.serviceCharge)

      mkSelectedFareCaption = "DRIVER_SELECTED_FARE"
      mbSelectedFareItem =
        fareParams.driverSelectedFare <&> \selFare ->
          mkBreakupItem mkSelectedFareCaption (mkPrice selFare)

      customerExtraFareCaption = "CUSTOMER_SELECTED_FARE"
      mkCustomerExtraFareItem =
        fareParams.customerExtraFee <&> \ceFare -> do
          mkBreakupItem customerExtraFareCaption (mkPrice ceFare)

      totalFareFinalRounded = fareSum fareParams
      totalFareCaption = "TOTAL_FARE"
      totalFareItem = mkBreakupItem totalFareCaption $ mkPrice totalFareFinalRounded

      nightShiftCaption = "NIGHT_SHIFT_CHARGE"
      mbNightShiftChargeItem = fmap (mkBreakupItem nightShiftCaption) (mkPrice <$> fareParams.nightShiftCharge)

      waitingChargesCaption = "WAITING_OR_PICKUP_CHARGES"
      mbWaitingChargesItem = mkBreakupItem waitingChargesCaption . mkPrice <$> fareParams.waitingCharge

      mbFixedGovtRateCaption = "FIXED_GOVERNMENT_RATE"
      mbFixedGovtRateItem = mkBreakupItem mbFixedGovtRateCaption . mkPrice <$> fareParams.govtCharges

      detailsBreakups = processFareParamsDetails dayPartRate fareParams.fareParametersDetails
  catMaybes
    [ Just totalFareItem,
      Just baseFareItem,
      Just baseFareDistanceItem,
      mbNightShiftChargeItem,
      mbWaitingChargesItem,
      mbFixedGovtRateItem,
      mbServiceChargeItem,
      mbSelectedFareItem,
      mkCustomerExtraFareItem
    ]
    <> detailsBreakups
  where
    processFareParamsDetails dayPartRate = \case
      DFParams.ProgressiveDetails det -> mkFPProgressiveDetailsBreakupList dayPartRate det
      DFParams.SlabDetails det -> mkFPSlabDetailsBreakupList det

    mkFPProgressiveDetailsBreakupList dayPartRate det = do
      let deadKmFareCaption = "DEAD_KILOMETER_FARE"
          deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice det.deadKmFare)

          extraDistanceFareCaption = "EXTRA_DISTANCE_FARE"
          mbExtraKmFareRounded = det.extraKmFare <&> roundToIntegral . (* dayPartRate) . fromIntegral -- temp fix :: have to fix properly
          extraDistanceFareItem =
            mbExtraKmFareRounded <&> \extraKmFareRounded ->
              mkBreakupItem extraDistanceFareCaption (mkPrice extraKmFareRounded)
      catMaybes [Just deadKmFareItem, extraDistanceFareItem]

    mkFPSlabDetailsBreakupList det = do
      let platformFeeCaption = "PLATFORM_FEE"
          mbPlatformFeeItem = mkBreakupItem platformFeeCaption . mkPrice <$> det.platformFee
          sgstCaption = "SGST"
          mbSgstItem = mkBreakupItem sgstCaption . mkPrice . roundToIntegral <$> det.sgst
          cgstCaption = "CGST"
          mbCgstItem = mkBreakupItem cgstCaption . mkPrice . roundToIntegral <$> det.cgst
      catMaybes [mbPlatformFeeItem, mbSgstItem, mbCgstItem]

-- TODO: make some tests for it

fareSum :: FareParameters -> Money
fareSum fareParams = do
  pureFareSum fareParams
    + fromMaybe 0 fareParams.driverSelectedFare
    + fromMaybe 0 fareParams.customerExtraFee

-- Pure fare without customerExtraFee and driverSelectedFare
pureFareSum :: FareParameters -> Money
pureFareSum fareParams = do
  let (partOfNightShiftCharge, notPartOfNightShiftCharge, platformFee, extraRentalFare) = countFullFareOfParamsDetails fareParams.fareParametersDetails
  fareParams.baseFare
    + fromMaybe 0 fareParams.serviceCharge
    + fromMaybe 0 fareParams.waitingCharge
    + fromMaybe 0 fareParams.govtCharges
    + fromMaybe 0 fareParams.nightShiftCharge
    + partOfNightShiftCharge
    + notPartOfNightShiftCharge
    + platformFee
    + extraRentalFare

data CalculateFareParametersParams = CalculateFareParametersParams
  { farePolicy :: FullFarePolicy,
    distance :: Meters,
    rideTime :: UTCTime,
    endRideTime :: Maybe UTCTime,
    waitingTime :: Maybe Minutes,
    driverSelectedFare :: Maybe Money,
    customerExtraFee :: Maybe Money,
    nightShiftCharge :: Maybe Money
  }

calculateFareParameters ::
  (Log m, MonadGuid m, MonadThrow m) =>
  CalculateFareParametersParams ->
  m FareParameters
calculateFareParameters params = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| params.farePolicy.merchantId ||+ " and vehicle variant " +|| params.farePolicy.vehicleVariant ||+ ""
  let fp = params.farePolicy
  id <- generateGUID
  let isNightShiftChargeIncluded = isNightShift <$> fp.nightShiftBounds <*> Just params.rideTime
      (baseFare, nightShiftCharge, waitingChargeInfo, fareParametersDetails) = processFarePolicyDetails fp.farePolicyDetails
      (partOfNightShiftCharge, notPartOfNightShiftCharge, _, rentalExtraFare) = countFullFareOfParamsDetails fareParametersDetails
      fullRideCost {-without govtCharges, platformFee, waitingCharge, notPartOfNightShiftCharge and nightShift-} =
        baseFare
          + fromMaybe 0 fp.serviceCharge
          + partOfNightShiftCharge
          + rentalExtraFare
  let resultNightShiftCharge = (\isCoefIncluded -> if isCoefIncluded then countNightShiftCharge fullRideCost <$> nightShiftCharge else Nothing) =<< isNightShiftChargeIncluded
      resultWaitingCharge = countWaitingCharge =<< waitingChargeInfo
      fullRideCostN {-without govtCharges and platformFee-} =
        fullRideCost
          + fromMaybe 0 resultNightShiftCharge
          + fromMaybe 0 resultWaitingCharge
          + notPartOfNightShiftCharge
      govtCharges =
        roundToIntegral . (fromIntegral fullRideCostN *) <$> (fp.govtCharges)
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
            waitingCharge = resultWaitingCharge,
            nightShiftCharge = resultNightShiftCharge,
            nightShiftRateIfApplies = (\isCoefIncluded -> if isCoefIncluded then getNightShiftRate nightShiftCharge else Nothing) =<< isNightShiftChargeIncluded, -- Temp fix :: have to fix properly
            fareParametersDetails = case fp.farePolicyDetails of
              DFP.ProgressiveDetails _ -> fareParametersDetails
              DFP.SlabsDetails det ->
                countPlatformFee -- Mb change platformFee from Nothing to proper value
                  fullCompleteRideCost
                  (DFP.findFPSlabsDetailsSlabByDistance params.distance det.slabs & (.platformFeeInfo))
                  fareParametersDetails,
            ..
          }
  logTagInfo "FareCalculator" $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
  where
    processFarePolicyDetails = \case
      DFP.ProgressiveDetails det -> processFPProgressiveDetails det
      DFP.SlabsDetails det -> processFPSlabsDetailsSlab $ DFP.findFPSlabsDetailsSlabByDistance params.distance det.slabs
    processFPProgressiveDetails DFP.FPProgressiveDetails {..} = do
      let mbExtraDistance =
            params.distance - baseDistance
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
        processFPProgressiveDetailsPerExtraKmFare' _ 0 = 0 :: Money
        processFPProgressiveDetailsPerExtraKmFare' sortedPerExtraKmFareSectionsLeft (extraDistanceLeft :: Meters) =
          case sortedPerExtraKmFareSectionsLeft of
            aSection :| [] -> roundToIntegral $ fromIntegral @_ @Float extraDistanceLeft * getPerExtraMRate aSection.perExtraKmRate
            aSection :| bSection : leftSections -> do
              let sectionDistance = bSection.startDistance - aSection.startDistance
                  extraDistanceWithinSection = min sectionDistance extraDistanceLeft
              roundToIntegral (fromIntegral @_ @Float extraDistanceWithinSection * getPerExtraMRate aSection.perExtraKmRate)
                + processFPProgressiveDetailsPerExtraKmFare' (bSection :| leftSections) (extraDistanceLeft - extraDistanceWithinSection)
        getPerExtraMRate perExtraKmRate = realToFrac @_ @Float perExtraKmRate / 1000

    processFPSlabsDetailsSlab DFP.FPSlabsDetailsSlab {..} = do
      ( baseFare,
        nightShiftCharge,
        waitingChargeInfo,
        DFParams.SlabDetails
          DFParams.FParamsSlabDetails
            { platformFee = Nothing, -- Nothing for now, can be counted only after everything else
              sgst = Nothing,
              cgst = Nothing
            }
        )

    countNightShiftCharge fullRideCost nightShiftCharge = do
      case nightShiftCharge of
        ProgressiveNightShiftCharge charge -> roundToIntegral $ (fromIntegral fullRideCost * charge) - fromIntegral fullRideCost
        ConstantNightShiftCharge charge -> charge

    getNightShiftRate nightShiftCharge = do
      -- Temp fix :: have to fix properly
      case nightShiftCharge of
        Just (ProgressiveNightShiftCharge charge) -> (Just . realToFrac) charge
        _ -> Nothing

    countWaitingCharge :: WaitingChargeInfo -> Maybe Money
    countWaitingCharge waitingChargeInfo = do
      let waitingTimeMinusFreeWatingTime = params.waitingTime <&> (\wt -> (-) wt waitingChargeInfo.freeWaitingTime)
      let chargedWaitingTime = if waitingTimeMinusFreeWatingTime < Just 0 then Nothing else waitingTimeMinusFreeWatingTime
      case waitingChargeInfo.waitingCharge of
        PerMinuteWaitingCharge charge -> (\waitingTime -> roundToIntegral $ fromIntegral waitingTime * charge) <$> chargedWaitingTime
        ConstantWaitingCharge charge -> Just charge -- Always charged, freeWaitingTime doesn't make sense in this case
    countPlatformFee :: Money -> Maybe PlatformFeeInfo -> FareParametersDetails -> FareParametersDetails
    countPlatformFee fullCompleteRideCost platformFeeInfo = \case
      (DFParams.ProgressiveDetails det) -> DFParams.ProgressiveDetails det -- should be impossible anyway
      (DFParams.SlabDetails _det) ->
        DFParams.SlabDetails $ maybe (FParamsSlabDetails Nothing Nothing Nothing) countPlatformFeeMath platformFeeInfo
      where
        countPlatformFeeMath platformFeeInfo' = do
          let baseFee = case platformFeeInfo'.platformFeeCharge of
                ProgressivePlatformFee charge -> fromIntegral fullCompleteRideCost * realToFrac charge
                ConstantPlatformFee charge -> fromIntegral charge
          FParamsSlabDetails
            { platformFee = Just . roundToIntegral $ baseFee,
              cgst = Just . realToFrac $ baseFee * platformFeeInfo'.cgst,
              sgst = Just . realToFrac $ baseFee * platformFeeInfo'.sgst
            }

countFullFareOfParamsDetails :: DFParams.FareParametersDetails -> (Money, Money, Money, Money)
countFullFareOfParamsDetails = \case
  DFParams.ProgressiveDetails det -> (fromMaybe 0 det.extraKmFare, det.deadKmFare, 0, 0) -- (partOfNightShiftCharge, notPartOfNightShiftCharge)
  DFParams.SlabDetails det -> (0, 0, fromMaybe 0 det.platformFee + roundToIntegral (fromMaybe 0 det.sgst + fromMaybe 0 det.cgst), 0)

isNightShift ::
  DFP.NightShiftBounds ->
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
