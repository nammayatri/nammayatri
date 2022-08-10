module SharedLogic.FareCalculator.OneWayFareCalculator.Flow
  ( OneWayFareParameters,
    ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
    fareSumWithDiscount,
    buildOneWayFareBreakups,
  )
where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Booking
import Domain.Types.FarePolicy.FareBreakup
import Domain.Types.FarePolicy.OneWayFarePolicy (OneWayFarePolicy)
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import SharedLogic.FareCalculator.OneWayFareCalculator.Calculator
  ( OneWayFareParameters (..),
    TripStartTime,
    calculateFareParameters,
    fareSum,
    fareSumWithDiscount,
  )
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy as FarePolicyS
import Tools.Error

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization -> Vehicle.Variant -> m (Maybe OneWayFarePolicy)
  }

serviceHandle :: EsqDBFlow m r => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId vehicleVariant -> do
        FarePolicyS.findOneWayFarePolicyByOrgAndVehicleVariant orgId vehicleVariant
    }

calculateFare ::
  EsqDBFlow m r =>
  Id Organization ->
  Vehicle.Variant ->
  HighPrecMeters ->
  UTCTime ->
  m OneWayFareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization ->
  Vehicle.Variant ->
  HighPrecMeters ->
  TripStartTime ->
  m OneWayFareParameters
doCalculateFare ServiceHandle {..} orgId vehicleVariant distance startTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance startTime
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams

buildOneWayFareBreakups :: MonadGuid m => OneWayFareParameters -> Id Booking -> m [FareBreakup]
buildOneWayFareBreakups fareParams bookingId = do
  baseFareBreakup <- buildBaseFareBreakup fareParams bookingId
  distanceFareBreakup <- buildDistanceFareBreakup fareParams bookingId
  discountFareBreakup <- buildDiscountFareBreakup fareParams.discount bookingId
  pure $ [baseFareBreakup, distanceFareBreakup] <> maybeToList discountFareBreakup

buildBaseFareBreakup :: MonadGuid m => OneWayFareParameters -> Id Booking -> m FareBreakup
buildBaseFareBreakup OneWayFareParameters {..} bookingId = do
  id <- Id <$> generateGUIDText
  let amount = roundToUnits $ nightShiftRate * baseFare
      description = "Base fare is " <> showRounded amount <> " rupees"
  pure FareBreakup {..}

buildDistanceFareBreakup :: MonadGuid m => OneWayFareParameters -> Id Booking -> m FareBreakup
buildDistanceFareBreakup OneWayFareParameters {..} bookingId = do
  id <- Id <$> generateGUIDText
  let amount = roundToUnits $ nightShiftRate * distanceFare
      description = "Distance fare is " <> showRounded amount <> " rupees"
  pure FareBreakup {..}

buildDiscountFareBreakup :: MonadGuid m => Maybe Amount -> Id Booking -> m (Maybe FareBreakup)
buildDiscountFareBreakup mbDiscount bookingId = do
  forM mbDiscount $ \discount -> do
    id <- Id <$> generateGUIDText
    let amount = roundToUnits $ negate discount -- this amount should be always below zero
        description = "Discount is " <> showRounded discount <> " rupees"
    pure FareBreakup {..}
