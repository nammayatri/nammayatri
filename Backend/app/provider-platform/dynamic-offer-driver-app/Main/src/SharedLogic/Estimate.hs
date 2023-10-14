{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module SharedLogic.Estimate
  ( module DEst,
    buildEstimate,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator

buildEstimate ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id DSR.SearchRequest ->
  UTCTime ->
  Meters ->
  Maybe Text ->
  FullFarePolicy ->
  m DEst.Estimate
buildEstimate searchReqId startTime dist specialLocationTag farePolicy = do
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = farePolicy,
          distance = dist,
          rideTime = startTime,
          endRideTime = Nothing,
          waitingTime = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = Nothing,
          nightShiftCharge = Nothing
        }
  let baseFare = fareSum fareParams
      estimateBreakups = mkBreakupList mkPrice mkBreakupItem fareParams
      additionalBreakups = mkAdditionalBreakups mkPrice mkBreakupItem dist farePolicy
  logDebug $ "baseFare: " <> show baseFare
  uuid <- generateGUID
  now <- getCurrentTime
  let mbDriverExtraFeeBounds = findDriverExtraFeeBoundsByDistance dist <$> farePolicy.driverExtraFeeBounds
  pure
    DEst.Estimate
      { id = Id uuid,
        requestId = searchReqId,
        vehicleVariant = farePolicy.vehicleVariant,
        minFare = baseFare + maybe 0 (.minFee) mbDriverExtraFeeBounds,
        maxFare = baseFare + maybe 0 (.maxFee) mbDriverExtraFeeBounds,
        estimateBreakupList =
          estimateBreakups <> additionalBreakups
            & filter (filterRequiredBreakups $ DFParams.getFareParametersType fareParams), -- TODO: Remove after roll out
        nightShiftInfo =
          ((fromMaybe 0 fareParams.nightShiftCharge,,) <$> getOldNightShiftCharge farePolicy.farePolicyDetails <*> farePolicy.nightShiftBounds)
            <&> \(nightShiftCharge, oldNightShiftCharge, nightShiftBounds) ->
              NightShiftInfo
                { nightShiftCharge = nightShiftCharge,
                  oldNightShiftCharge = oldNightShiftCharge,
                  nightShiftStart = nightShiftBounds.nightShiftStart,
                  nightShiftEnd = nightShiftBounds.nightShiftEnd
                },
        waitingCharges = makeWaitingCharges,
        specialLocationTag = specialLocationTag,
        createdAt = now
      }
  where
    currency = "INR"
    mkPrice = DEst.EstimateBreakupPrice currency
    mkBreakupItem = DEst.EstimateBreakup
    makeWaitingCharges = do
      let waitingChargeInfo = case farePolicy.farePolicyDetails of
            SlabsDetails det -> (findFPSlabsDetailsSlabByDistance dist det.slabs).waitingChargeInfo
            ProgressiveDetails det -> det.waitingChargeInfo
      let (mbWaitingChargePerMin, mbWaitingOrPickupCharges) = case waitingChargeInfo <&> (.waitingCharge) of
            Just (PerMinuteWaitingCharge charge) -> (Just $ roundToIntegral charge, Nothing)
            Just (ConstantWaitingCharge charge) -> (Nothing, Just charge)
            Nothing -> (Nothing, Nothing)
      WaitingCharges
        { waitingChargePerMin = mbWaitingChargePerMin,
          waitingOrPickupCharges = mbWaitingOrPickupCharges
        }
    getOldNightShiftCharge farePolicyDetails = do
      let getNightShiftChargeValue (DFP.ProgressiveNightShiftCharge a) = realToFrac a --TODO Doesn't make sense, to be removed
          getNightShiftChargeValue (DFP.ConstantNightShiftCharge a) = fromIntegral a
      case farePolicyDetails of
        DFP.SlabsDetails det -> getNightShiftChargeValue <$> (DFP.findFPSlabsDetailsSlabByDistance dist det.slabs).nightShiftCharge
        DFP.ProgressiveDetails det -> getNightShiftChargeValue <$> det.nightShiftCharge
    filterRequiredBreakups fParamsType breakup = do
      case fParamsType of
        DFParams.Progressive ->
          breakup.title == "BASE_DISTANCE_FARE"
            || breakup.title == "EXTRA_PER_KM_FARE"
            || breakup.title == "DEAD_KILOMETER_FARE"
            || breakup.title == "DRIVER_MIN_EXTRA_FEE"
            || breakup.title == "DRIVER_MAX_EXTRA_FEE"
        DFParams.Slab ->
          breakup.title == "BASE_DISTANCE_FARE"
            || breakup.title == "SERVICE_CHARGE"
            || breakup.title == "WAITING_OR_PICKUP_CHARGES"
            || breakup.title == "PLATFORM_FEE"
            || breakup.title == "SGST"
            || breakup.title == "CGST"
            || breakup.title == "FIXED_GOVERNMENT_RATE"
            || breakup.title == "NIGHT_SHIFT_CHARGE"

mkAdditionalBreakups :: (Money -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> Meters -> FullFarePolicy -> [breakupItem]
mkAdditionalBreakups mkPrice mkBreakupItem distance farePolicy = do
  let driverExtraFeeBounds = findDriverExtraFeeBoundsByDistance distance <$> farePolicy.driverExtraFeeBounds
  let driverMinExtraFee = driverExtraFeeBounds <&> (.minFee)
      driverMinExtraFeeCaption = "DRIVER_MIN_EXTRA_FEE"
      driverMinExtraFeeItem = mkBreakupItem driverMinExtraFeeCaption . mkPrice <$> driverMinExtraFee

      driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee)
      driverMaxExtraFeeCaption = "DRIVER_MAX_EXTRA_FEE"
      driverMaxExtraFeeItem = mkBreakupItem driverMaxExtraFeeCaption . mkPrice <$> driverMaxExtraFee

      additionalDetailsBreakups = processAdditionalDetails farePolicy.farePolicyDetails
  catMaybes
    [ driverMinExtraFeeItem,
      driverMaxExtraFeeItem
    ]
    <> additionalDetailsBreakups
  where
    processAdditionalDetails = \case
      DFP.ProgressiveDetails det -> mkAdditionalProgressiveBreakups det
      DFP.SlabsDetails det -> mkAdditionalSlabBreakups $ DFP.findFPSlabsDetailsSlabByDistance distance det.slabs
    mkAdditionalProgressiveBreakups det = do
      let (perExtraKmFareSection :| _) = NE.sortBy (comparing (.startDistance)) det.perExtraKmRateSections
          perExtraKmFareCaption = "EXTRA_PER_KM_FARE"
          perExtraKmFareItem = mkBreakupItem perExtraKmFareCaption (mkPrice $ roundToIntegral perExtraKmFareSection.perExtraKmRate)

      let waitingOrPickupChargesCaption = "WAITING_OR_PICKUP_CHARGES"
          mbWatingChargeValue =
            (det.waitingChargeInfo <&> (.waitingCharge)) <&> \case
              PerMinuteWaitingCharge hpm -> roundToIntegral hpm
              ConstantWaitingCharge mo -> mo
          mbWaitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption . mkPrice <$> mbWatingChargeValue
      [perExtraKmFareItem] <> catMaybes [mbWaitingOrPickupChargesItem]
    mkAdditionalSlabBreakups det = do
      let waitingOrPickupChargesCaption = "WAITING_OR_PICKUP_CHARGES"
          mbWatingChargeValue =
            (det.waitingChargeInfo <&> (.waitingCharge)) <&> \case
              PerMinuteWaitingCharge hpm -> roundToIntegral hpm
              ConstantWaitingCharge mo -> mo
          mbWaitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption . mkPrice <$> mbWatingChargeValue
      catMaybes [mbWaitingOrPickupChargesItem]
