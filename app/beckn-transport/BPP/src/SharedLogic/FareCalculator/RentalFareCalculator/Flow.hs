module SharedLogic.FareCalculator.RentalFareCalculator.Flow
  ( RentalFareParameters,
    ServiceHandle (..),
    calculateRentalFare,
    doCalculateRentalFare,
    rentalFareSum,
    rentalFareSumWithDiscount,
    buildRentalFareBreakups,
  )
where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Booking (Booking)
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import EulerHS.Prelude hiding (id)
import SharedLogic.FareCalculator.RentalFareCalculator.Calculator
  ( RentalFareParameters,
    calculateRentalFareParameters,
    rentalFareSum,
    rentalFareSumWithDiscount,
  )
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as QRentalFP
import Tools.Error

type MonadHandler m = (MonadThrow m, Log m, MonadGuid m)

newtype ServiceHandle m = ServiceHandle
  { getRentalFarePolicy :: Id DRentalFP.RentalFarePolicy -> m (Maybe DRentalFP.RentalFarePolicy)
  }

serviceHandle :: EsqDBFlow m r => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getRentalFarePolicy = \rentalFarePolicyId -> do
        QRentalFP.findById rentalFarePolicyId
    }

calculateRentalFare ::
  EsqDBFlow m r =>
  Id DRentalFP.RentalFarePolicy ->
  HighPrecMeters ->
  UTCTime ->
  UTCTime ->
  m RentalFareParameters
calculateRentalFare = doCalculateRentalFare serviceHandle

doCalculateRentalFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id DRentalFP.RentalFarePolicy ->
  HighPrecMeters ->
  UTCTime ->
  UTCTime ->
  m RentalFareParameters
doCalculateRentalFare ServiceHandle {..} rentalFarePolicyId distance startTime stopTime = do
  rentalFarePolicy <-
    getRentalFarePolicy rentalFarePolicyId
      >>= fromMaybeM NoRentalFarePolicy
  logTagInfo "RentalFareCalculator" $
    "Initiating rental fare calculation for organization "
      +|| rentalFarePolicy.organizationId ||+ " for "
      +|| rentalFarePolicy.vehicleVariant ||+ ""
  let fareParams = calculateRentalFareParameters rentalFarePolicy distance startTime stopTime
  logTagInfo
    "RentalFareCalculator"
    $ "Rental fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams

buildRentalFareBreakups :: MonadGuid m => RentalFareParameters -> Id Booking -> m [FareBreakup]
buildRentalFareBreakups fareParams bookingId = do
  baseFareBreakup <- buildBaseFareBreakup fareParams bookingId
  extraDistanceFareBreakup <- buildExtraDistanceFareBreakup fareParams bookingId
  extraTimeFareBreakup <- buildExtraTimeFareBreakup fareParams bookingId
  nextDaysFareBreakup <- buildNextDaysFareBreakup fareParams bookingId
  discountFareBreakup <- buildDiscountFareBreakup fareParams.discount bookingId
  pure $ [baseFareBreakup, extraDistanceFareBreakup, extraTimeFareBreakup] <> catMaybes [nextDaysFareBreakup, discountFareBreakup]

buildBaseFareBreakup :: MonadGuid m => RentalFareParameters -> Id Booking -> m FareBreakup
buildBaseFareBreakup fareParams bookingId = do
  id <- Id <$> generateGUIDText
  let amount = roundToUnits fareParams.baseFare
      description =
        "Base fare for "
          <> show fareParams.farePolicy.baseDistance
          <> " km and "
          <> show fareParams.farePolicy.baseDuration
          <> " hours is "
          <> showRounded amount
          <> " rupees"
  pure FareBreakup {..}

buildExtraDistanceFareBreakup :: MonadGuid m => RentalFareParameters -> Id Booking -> m FareBreakup
buildExtraDistanceFareBreakup fareParams bookingId = do
  id <- Id <$> generateGUIDText
  let amount = roundToUnits fareParams.extraDistanceFare
      description =
        "Extra distance fare with fare policy "
          <> showRounded fareParams.farePolicy.extraKmFare
          <> " rupees per km is "
          <> showRounded amount
          <> " rupees"
  pure FareBreakup {..}

buildExtraTimeFareBreakup :: MonadGuid m => RentalFareParameters -> Id Booking -> m FareBreakup
buildExtraTimeFareBreakup fareParams bookingId = do
  id <- Id <$> generateGUIDText
  let amount = roundToUnits fareParams.extraTimeFare
      description =
        "Extra time fare with fare policy "
          <> showRounded fareParams.farePolicy.extraMinuteFare
          <> " rupees per minute is "
          <> showRounded amount
          <> " rupees"
  pure FareBreakup {..}

buildNextDaysFareBreakup :: MonadGuid m => RentalFareParameters -> Id Booking -> m (Maybe FareBreakup)
buildNextDaysFareBreakup fareParams bookingId = do
  let mbAmount = roundToUnits <$> fareParams.nextDaysFare
  forM mbAmount $ \amount -> do
    id <- Id <$> generateGUIDText
    let description =
          "Next days fare with fare policy "
            <> showRounded (fromMaybe 0 fareParams.farePolicy.driverAllowanceForDay)
            <> " rupees per next day is "
            <> showRounded amount
            <> " rupees"
    pure FareBreakup {..}

buildDiscountFareBreakup :: MonadGuid m => Maybe Amount -> Id Booking -> m (Maybe FareBreakup)
buildDiscountFareBreakup mbDiscount bookingId = do
  forM mbDiscount $ \discount -> do
    id <- Id <$> generateGUIDText
    let amount = roundToUnits $ negate discount -- this amount should be always below zero
        description = "Discount is " <> showRounded discount <> " rupees"
    pure FareBreakup {..}
