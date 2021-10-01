module Product.FareCalculator.Interpreter (calculateFare) where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Product.FareCalculator.Flow
  ( DropLocation (DropLocation),
    JourneyTrip (OneWayTrip),
    PickupLocation (PickupLocation),
    ServiceHandle (..),
    doCalculateFare,
    fareSum,
  )
import qualified Product.Location as Location
import qualified Storage.Queries.FarePolicy as FarePolicyS
import Types.Metrics (CoreMetrics)
import Types.Storage.Organization (Organization)
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common

calculateFare ::
  ( DBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Organization ->
  Vehicle.Variant ->
  Double ->
  UTCTime ->
  m Amount
calculateFare orgId vehicleVariant distanceSrc startTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ " for " +|| vehicleVariant ||+ ""
  fareParams <-
    doCalculateFare
      serviceHandle
      orgId
      vehicleVariant
      distanceSrc
      OneWayTrip -- TODO :: determine the type of trip
      startTime
  let totalFare = fareSum fareParams
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ". Total fare: " +|| totalFare ||+ ""
  pure totalFare

serviceHandle ::
  ( DBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \orgId vehicleVariant -> do
        FarePolicyS.findFarePolicyByOrgAndVehicleVariant orgId vehicleVariant,
      getDistance = \(PickupLocation pickupLoc) (DropLocation dropLoc) -> do
        let pickupLocLatLong = Location.locationToLatLong pickupLoc
            dropLocLatLong = Location.locationToLatLong dropLoc
        Location.calculateDistance pickupLocLatLong dropLocLatLong
    }