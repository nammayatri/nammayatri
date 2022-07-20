module SharedLogic.FareCalculator.OneWayFareCalculator.Flow
  ( OneWayFareParameters (..),
    ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
    fareSumWithDiscount,
    buildOneWayFareBreakups,
  )
where

import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.FarePolicy.FareBreakup
import Domain.Types.FarePolicy.OneWayFarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import Domain.Types.RideBooking (RideBooking)
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
import Types.Error
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization -> Vehicle.Variant -> m (Maybe FarePolicy)
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

buildOneWayFareBreakups :: MonadGuid m => OneWayFareParameters -> Id RideBooking -> m [FareBreakup]
buildOneWayFareBreakups fareParams rideBookingId = do
  baseFareBreakup <- buildBaseFareBreakup fareParams rideBookingId
  distanceFareBreakup <- buildDistanceFareBreakup fareParams rideBookingId
  discountFareBreakup <- buildDiscountFareBreakup fareParams.discount rideBookingId
  pure $ [baseFareBreakup, distanceFareBreakup] <> maybeToList discountFareBreakup

buildBaseFareBreakup :: MonadGuid m => OneWayFareParameters -> Id RideBooking -> m FareBreakup
buildBaseFareBreakup OneWayFareParameters {..} rideBookingId = do
  id <- Id <$> generateGUIDText
  let amount = nightShiftRate * baseFare
      description = "Base fare is " <> show amount <> "rupees"
  pure FareBreakup {..}

buildDistanceFareBreakup :: MonadGuid m => OneWayFareParameters -> Id RideBooking -> m FareBreakup
buildDistanceFareBreakup OneWayFareParameters {..} rideBookingId = do
  id <- Id <$> generateGUIDText
  let amount = nightShiftRate * distanceFare
      description = "Distance fare is " <> show amount <> " rupees"
  pure FareBreakup {..}

buildDiscountFareBreakup :: MonadGuid m => Maybe Amount -> Id RideBooking -> m (Maybe FareBreakup)
buildDiscountFareBreakup mbDiscount rideBookingId = do
  forM mbDiscount $ \discount -> do
    id <- Id <$> generateGUIDText
    let amount = negate discount -- this amount should be always below zero
        description = "Discount is " <> show discount <> " rupees"
    pure FareBreakup {..}
