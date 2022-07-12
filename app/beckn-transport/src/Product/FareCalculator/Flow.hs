module Product.FareCalculator.Flow
  ( FareParameters (..),
    ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
    fareSumWithDiscount,
    buildFareBreakups,
  )
where

import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.FareBreakup
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import Domain.Types.RideBooking (RideBooking)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Calculator
  ( FareParameters (..),
    TripStartTime,
    calculateFareParameters,
    fareSum,
    fareSumWithDiscount,
  )
import qualified Storage.Queries.FarePolicy as FarePolicyS
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
        FarePolicyS.findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant
    }

calculateFare ::
  EsqDBFlow m r =>
  Id Organization ->
  Vehicle.Variant ->
  HighPrecMeters ->
  UTCTime ->
  m FareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization ->
  Vehicle.Variant ->
  HighPrecMeters ->
  TripStartTime ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId vehicleVariant distance startTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  farePolicy <- getFarePolicy orgId vehicleVariant >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance startTime
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams

buildFareBreakups :: MonadGuid m => FareParameters -> Id RideBooking -> m [FareBreakup]
buildFareBreakups fareParams rideBookingId = do
  baseFareBreakup <- buildBaseFareBreakup fareParams rideBookingId
  distanceFareBreakup <- buildDistanceFareBreakup fareParams rideBookingId
  discountFareBreakup <- buildDiscountFareBreakup fareParams.discount rideBookingId
  pure $ [baseFareBreakup, distanceFareBreakup] <> maybeToList discountFareBreakup

buildBaseFareBreakup :: MonadGuid m => FareParameters -> Id RideBooking -> m FareBreakup
buildBaseFareBreakup FareParameters {..} rideBookingId = do
  id <- Id <$> generateGUIDText
  let amount = nightShiftRate * baseFare
      description = "Base fare is " <> show amount <> "rupees"
  pure FareBreakup {..}

buildDistanceFareBreakup :: MonadGuid m => FareParameters -> Id RideBooking -> m FareBreakup
buildDistanceFareBreakup FareParameters {..} rideBookingId = do
  id <- Id <$> generateGUIDText
  let amount = nightShiftRate * distanceFare
      description = "Distance fare is " <> show amount <> " rupees"
  pure FareBreakup {..}

buildDiscountFareBreakup :: MonadGuid m => Maybe Amount -> Id RideBooking -> m (Maybe FareBreakup)
buildDiscountFareBreakup mbDiscount rideBookingId = do
  forM mbDiscount $ \discount -> do
    id <- Id <$> generateGUIDText
    let amount = -discount -- this amount should be always below zero
        description = "Discount is " <> show discount <> " rupees"
    pure FareBreakup {..}
