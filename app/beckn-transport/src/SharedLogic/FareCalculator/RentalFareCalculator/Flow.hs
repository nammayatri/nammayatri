module SharedLogic.FareCalculator.RentalFareCalculator.Flow
  ( RentalFareParameters (..),
    ServiceHandle (..),
    calculateRentalFare,
    doCalculateRentalFare,
    rentalFareSum,
    rentalFareSumWithDiscount,
    buildRentalFareBreakups,
  )
where

import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import Domain.Types.RideBooking (RideBooking)
import EulerHS.Prelude hiding (id)
import SharedLogic.FareCalculator.RentalFareCalculator.Calculator
  ( RentalFareParameters (..),
    calculateRentalFareParameters,
    rentalFareSum,
    rentalFareSumWithDiscount,
  )
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as QRentalFP
import Types.Error
import Utils.Common

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

buildRentalFareBreakups :: MonadGuid m => RentalFareParameters -> Id RideBooking -> m [FareBreakup]
buildRentalFareBreakups fareParams rideBookingId = do
  baseFareBreakup <- buildBaseFareBreakup fareParams rideBookingId
  extraDistanceFareBreakup <- buildExtraDistanceFareBreakup fareParams rideBookingId
  extraTimeFareBreakup <- buildExtraTimeFareBreakup fareParams rideBookingId
  nextDaysFareBreakup <- buildNextDaysFareBreakup fareParams rideBookingId
  discountFareBreakup <- buildDiscountFareBreakup fareParams.discount rideBookingId
  pure $ [baseFareBreakup, extraDistanceFareBreakup, extraTimeFareBreakup] <> catMaybes [nextDaysFareBreakup, discountFareBreakup]

buildBaseFareBreakup :: MonadGuid m => RentalFareParameters -> Id RideBooking -> m FareBreakup
buildBaseFareBreakup fareParams rideBookingId = do
  id <- Id <$> generateGUIDText
  let amount = fareParams.baseFare
      description =
        "Base fare for "
          <> show fareParams.farePolicy.baseDistance
          <> " km and "
          <> show fareParams.farePolicy.baseDuration
          <> " hours is "
          <> show amount
          <> " rupees"
  pure FareBreakup {..}

buildExtraDistanceFareBreakup :: MonadGuid m => RentalFareParameters -> Id RideBooking -> m FareBreakup
buildExtraDistanceFareBreakup fareParams rideBookingId = do
  id <- Id <$> generateGUIDText
  let amount = fareParams.extraDistanceFare
      description =
        "Extra distance fare with fare policy "
          <> show fareParams.farePolicy.extraKmFare
          <> " rupees per km is "
          <> show amount
          <> " rupees"
  pure FareBreakup {..}

buildExtraTimeFareBreakup :: MonadGuid m => RentalFareParameters -> Id RideBooking -> m FareBreakup
buildExtraTimeFareBreakup fareParams rideBookingId = do
  id <- Id <$> generateGUIDText
  let amount = fareParams.extraTimeFare
      description =
        "Extra time fare with fare policy "
          <> show fareParams.farePolicy.extraMinuteFare
          <> " rupees per minute is "
          <> show amount
          <> " rupees"
  pure FareBreakup {..}

buildNextDaysFareBreakup :: MonadGuid m => RentalFareParameters -> Id RideBooking -> m (Maybe FareBreakup)
buildNextDaysFareBreakup fareParams rideBookingId = do
  let mbAmount = fareParams.nextDaysFare
  forM mbAmount $ \amount -> do
    id <- Id <$> generateGUIDText
    let description =
          "Next days fare with fare policy "
            <> show fareParams.farePolicy.driverAllowanceForDay
            <> " rupees per next day is "
            <> show amount
            <> " rupees"
    pure FareBreakup {..}

buildDiscountFareBreakup :: MonadGuid m => Maybe Amount -> Id RideBooking -> m (Maybe FareBreakup)
buildDiscountFareBreakup mbDiscount rideBookingId = do
  forM mbDiscount $ \discount -> do
    id <- Id <$> generateGUIDText
    let amount = negate discount -- this amount should be always below zero
        description = "Discount is " <> show discount <> " rupees"
    pure FareBreakup {..}
