module Product.FareCalculator.Flow
  ( FareParameters (..),
    ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
    fareSumWithDiscount,
  )
where

import Beckn.Types.Id
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
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
